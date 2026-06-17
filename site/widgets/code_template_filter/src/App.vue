<script setup lang="ts">
import { computed, inject, ref } from 'vue'
import { useToast } from 'vue-toastification';
import Fuse from 'fuse.js'
import CodeShow from './components/codeShow.vue'

interface CodeTemplate {
  title?: string;
  tags?: string[];
  code: string;
  desc?: string;
  sh?: string;
}

const templates = inject<CodeTemplate[]>('template_array', [])
const searchText = ref('')
const toast = useToast()

const showCodeModal = ref(false)
const modalTitle = ref('')
const modalCode = ref('')
const modalFilename = ref('')

const fuse = new Fuse(templates, {
  keys: ['title', 'desc', 'tags']
})

function codeAssetUrl(codePath: string) {
  const baseUrl = import.meta.env.DEV ? '/' : '/code_template/';
  return baseUrl + codePath;
}

function fileNameFromPath(codePath: string) {
  return codePath.split('/').pop() || 'code.txt';
}

function tagsString(tags: string[] = []) {
  return tags.join(',');
}

async function fetchCode(codeFileUrl: string) {
  try {
    const response = await fetch(codeAssetUrl(codeFileUrl));
    if (!response.ok) {
      throw new Error(`HTTP ${response.status}`);
    }

    return await response.text();
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    toast.error(`获取代码失败: ${message}`);
    return null;
  }
}

async function viewCode(item: CodeTemplate) {
  const code = await fetchCode(item.code);
  if (!code) return;

  modalTitle.value = item.title || fileNameFromPath(item.code);
  modalFilename.value = fileNameFromPath(item.code);
  modalCode.value = code;
  showCodeModal.value = true;
}

function closeCodeModal() {
  showCodeModal.value = false;
  modalCode.value = '';
  modalTitle.value = '';
  modalFilename.value = '';
}

function downloadCode(item: CodeTemplate) {
  const link = document.createElement('a');
  const filename = fileNameFromPath(item.code);
  link.href = codeAssetUrl(item.code);
  link.setAttribute('download', filename);

  document.body.appendChild(link);
  link.click();
  document.body.removeChild(link);
  toast.success(`开始下载 ${filename}`);
}

async function copyToClipboard(text: string, successMessage: string) {
  try {
    if (!navigator.clipboard || !window.isSecureContext) {
      throw new Error('浏览器不支持剪贴板 API');
    }

    await navigator.clipboard.writeText(text);
    toast.success(successMessage);
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    toast.error(`无法复制: ${message}`);
  }
}

async function copyTemplateCode(item: CodeTemplate) {
  const code = await fetchCode(item.code);
  if (code) {
    await copyToClipboard(code, '复制代码成功!');
  }
}

async function copyShell(item: CodeTemplate) {
  if (item.sh) {
    await copyToClipboard(item.sh, '成功,请复制到终端里执行!');
  }
}

const searchResult = computed(() => {
  const keyword = searchText.value.trim();
  return keyword ? fuse.search(keyword).map(result => result.item) : templates;
})
</script>

<template>
    <div class="container">
      <h1>代码模板过滤器</h1>
        <div class="search_box">
            <div class="input-group flex-nowrap">
                <span class="input-group-text" id="addon-wrapping">搜索</span>
                <input v-model="searchText" type="text" class="form-control" placeholder="请输入内容" aria-label="search" aria-describedby="addon-wrapping">
            </div>
        </div>

        <div class="">
             <!-- table -->
            <table class="table table-striped table-hover">
                <thead>
                    <tr>
                        <th scope="col">#</th>
                        <th scope="col">标题</th>
                        <th scope="col">描述</th>
                        <th scope="col">标签</th>
                        <th scope="col">功能</th>
                    </tr>
                </thead>
                <tbody>
                    <tr v-for="(d,index) in searchResult" :key="d.code">
                        <th scope="row">{{index+1}}</th>
                        <td>{{d.title}}</td>
                        <td style="max-width: 300px;">{{d.desc || "" }}</td>
                        <td>{{ tagsString(d.tags)}}</td>
                        <td>
                          <div class="btn-group">
                            <button @click="viewCode(d)" class="">查看</button>
                            <button @click="downloadCode(d)" class="">下载</button>
                            <button @click="copyTemplateCode(d)" class="">复制</button>
                            <button @click="copyShell(d)" class="" v-show="d.sh">命令</button>
                          </div>
                        </td>
                    </tr>
                </tbody>
            </table>
        </div>
        
        <!-- 代码查看模态框 -->
        <CodeShow 
            :isVisible="showCodeModal"
            :title="modalTitle"
            :code="modalCode"
            :filename="modalFilename"
            @close="closeCodeModal"
        />
    </div>
</template>

<style scoped>
:global(body) {
  background-color: #1a1a1a;
  color: #FFC700; /* Amber color */
  font-family: 'Courier New', Courier, monospace;
}

.container {
    background-color: #000;
    max-width: 1000px;
    margin: 2rem auto;
    padding: 2rem;
    /* border: 1px solid #FFC700; */
    /* box-shadow: 0 0 15px rgba(255, 199, 0, 0.5); */
}

h1 {
    color: #FFD700;
    text-align: center;
    margin-bottom: 2rem;
    text-transform: uppercase;
    letter-spacing: 0.2em;
    font-weight: bold;
    text-shadow: 0 0 5px #FFC700;
}

.search_box {
    margin-bottom: 2rem;
}

.input-group, .form-control, .input-group-text {
    background-color: transparent;
    color: #FFC700;
    border: 1px solid #FFC700;
    border-radius: 0;
}

.form-control {
    background-color: #111;
}

.form-control::placeholder {
    color: #997a00;
}

.form-control:focus {
    background-color: #222;
    color: #FFD700;
    box-shadow: 0 0 8px #FFC700;
    border-color: #FFD700;
}

.input-group-text {
    background-color: #332a00;
    font-weight: bold;
}

.table {
  width: 100%;
  margin-bottom: 1rem;
  color: #FFC700;
  border-collapse: collapse;
  text-align: left;
  border: 1px solid #FFC700;
}

.table th,
.table td {
  padding: 0.75rem;
  font-size: medium;
  vertical-align: middle;
  border: 1px dotted #554400;
}

.table thead th {
  vertical-align: bottom;
  border-bottom: 2px solid #FFC700;
  background-color: transparent;
  color: #FFD700; /* Brighter for headers */
  text-transform: uppercase;
}

.table tbody tr:hover td {
    background-color: #221c00;
}

.btn-group {
    display: flex;
    gap: 0.5rem;
}

.btn-group button {
    padding: 0.375rem 0.75rem;
    font-size: 1rem;
    border-radius: 0;
    border: 1px solid #FFC700;
    cursor: pointer;
    color: #FFC700;
    background-color: #111;
    text-transform: uppercase;
    transition: all 0.2s ease-in-out;
}

.btn-group button:hover {
    background-color: #FFC700;
    color: #000;
    box-shadow: 0 0 10px #FFC700;
}
</style>
