<script setup>
import {inject,ref,computed} from 'vue'
import { useToast } from "vue-toastification";
  // import HelloWorld from './components/HelloWorld.vue'
// import tableCell from './components/tableCell.vue'
import Fuse from 'fuse.js'
import codeShow from './components/codeShow.vue'

const template_array = inject('template_array')
const search_text = ref("")
const toast = useToast()

// 筛选相关状态
const selectedOJs = ref([])
const selectedTags = ref([])
const showFilters = ref(false)

// 模态框相关状态
const showCodeModal = ref(false)
const modalTitle = ref('')
const modalCode = ref('')
const modalFilename = ref('')

const FuseOptions = {
	// isCaseSensitive: false,
	// includeScore: false,
	// shouldSort: true,
	// includeMatches: false,
	// findAllMatches: false,
	// minMatchCharLength: 1,
	// location: 0,
	// threshold: 0.6,
	// distance: 100,
	// useExtendedSearch: false,
	// ignoreLocation: false,
	// ignoreFieldNorm: false,
	// fieldNormWeight: 1,
	keys: [
		"title","problem_id","oj","desc"
	]
}

const fuse = new Fuse(template_array,FuseOptions)

// 获取所有唯一的OJ列表
const uniqueOJs = computed(() => {
    const ojs = new Set()
    template_array.forEach(item => {
        if (item.oj) ojs.add(item.oj)
    })
    return Array.from(ojs).sort()
})

// 获取所有唯一的tag列表
const uniqueTags = computed(() => {
    const tags = new Set()
    template_array.forEach(item => {
        if (item.tags && Array.isArray(item.tags)) {
            item.tags.forEach(tag => tags.add(tag))
        }
    })
    return Array.from(tags).sort()
})

const tags_string = (tags) => {
    return tags.join(",")
}

const fetch_code = (codeFileUrl) => {
    const baseUrl = import.meta.env.DEV ? '/' : '/code_template/';
    const fullUrl = baseUrl + codeFileUrl;
    return fetch(fullUrl)
    .then(response => {
        if (response.ok) {
            return response.text();
        } else {
            throw new Error('Failed to fetch the file');
        }
    })
    .catch(error => {
        toast.error(`Error fetching code: ${error.message}`);
        return null;
    });
}

const view_code = (item) => {
    modalTitle.value = item.title;
    modalFilename.value = item.code.split('/').pop();
    
    fetch_code(item.code).then(code => {
        if (code) {
            modalCode.value = code;
            showCodeModal.value = true;
        }
    }).catch(error => {
        toast.error('获取代码失败: ' + error.message);
    });
}

const closeCodeModal = () => {
    showCodeModal.value = false;
    modalCode.value = '';
    modalTitle.value = '';
    modalFilename.value = '';
}

const download_code = (item) => {
    const codeUrl = '/code_template/' + item.code;
    const link = document.createElement('a');
    link.href = codeUrl;
    
    const filename = item.code.split('/').pop();
    link.setAttribute('download', filename || 'download');
    
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
    toast.success(`开始下载 ${filename}`);
}

const copy_to_clipboard = (text, successMessage) => {
  if (navigator.clipboard && window.isSecureContext) {
    navigator.clipboard.writeText(text).then(() => {
      toast.success(successMessage);
    }).catch(err => {
      toast.error('无法复制: ' + err);
    });
  } else {
    toast.error('浏览器不支持剪贴板API');
  }
};

const template_code_copy = (item) => {
    let code_url = item.code
    fetch_code(code_url).then( code=> {
      console.log(code_url)
      console.log(code)
        if (code) {
            copy_to_clipboard(code, '复制代码成功!');
        }
    })
}

const copy_shell = (item) => {
    copy_to_clipboard(item.sh, '成功,请复制到终端里执行!');
}

const search_func = () => {
    console.log(search_text.value)
    if(search_text.value.length == 0) {
        toast.warning("请输入内容!")
        return;
    }
    toast.info("TODO,等待完成!")
    let ret = fuse.search(search_text.value);
    console.log(ret)
}

const search_result = computed(
    () =>{
        let result;
        if( search_text.value.length == 0)
            result = template_array
        else
            result = fuse.search(search_text.value).map(r => r.item);
        
        // OJ筛选
        if (selectedOJs.value.length > 0) {
            result = result.filter(item => 
                selectedOJs.value.includes(item.oj)
            )
        }
        
        // Tag筛选
        if (selectedTags.value.length > 0) {
            result = result.filter(item => 
                item.tags && selectedTags.value.some(tag => item.tags.includes(tag))
            )
        }
        
        // 按日期从新到旧排序
        return result.sort((a, b) => {
            // 将日期字符串转换为时间戳进行比较
            const dateA = new Date(a.date).getTime();
            const dateB = new Date(b.date).getTime();
            return dateB - dateA; // 降序排列（从新到旧）
        });
    }
)
</script>

<template>
    <div class="container">
      <h1>题目列表</h1>
        <div class="search_box">
            <div class="input-group flex-nowrap">
                <span class="input-group-text" id="addon-wrapping">搜索</span>
                <!-- <input @keydown.enter="search_func"  v-model="search_text" type="text" class="form-control" placeholder="请输入内容" aria-label="search" aria-describedby="addon-wrapping"> -->
                <input  v-model="search_text" type="text" class="form-control" placeholder="请输入内容" aria-label="search" aria-describedby="addon-wrapping">
            </div>
        </div>

        <!-- 筛选区域 -->
        <div class="filter-section">
            <div class="filter-header" @click="showFilters = !showFilters">
                <span>筛选选项</span>
                <span class="filter-toggle">{{ showFilters ? '▼' : '▶' }}</span>
            </div>
            
            <div v-show="showFilters" class="filter-content">
                <!-- OJ筛选 -->
                <div class="filter-group">
                    <h4>OJ平台</h4>
                    <div class="checkbox-grid">
                        <label v-for="oj in uniqueOJs" :key="oj" class="checkbox-label">
                            <input 
                                type="checkbox" 
                                :value="oj" 
                                v-model="selectedOJs"
                            >
                            <span>{{ oj }}</span>
                        </label>
                    </div>
                </div>
                
                <!-- Tag筛选 -->
                <div class="filter-group">
                    <h4>标签</h4>
                    <div class="checkbox-grid">
                        <label v-for="tag in uniqueTags" :key="tag" class="checkbox-label">
                            <input 
                                type="checkbox" 
                                :value="tag" 
                                v-model="selectedTags"
                            >
                            <span>{{ tag }}</span>
                        </label>
                    </div>
                </div>
                
                <!-- 清除筛选按钮 -->
                <div class="filter-actions">
                    <button @click="selectedOJs = []; selectedTags = []" class="clear-filters-btn">
                        清除所有筛选
                    </button>
                </div>
            </div>
        </div>

        <div class="">
            <!-- <tableCell :cell="d.item || d" v-for="d in search_result"/> -->
             <!-- table -->
            <table class="table table-striped table-hover">
                <thead>
                    <tr>
                        <th scope="col">#</th>
                        <th scope="col">标题</th>
                        <th scope="col">简要描述</th>
                        <th scope="col">OJ</th>
                        <th scope="col">标签</th>
                        <th scope="col">时间</th>
                    </tr>
                </thead>
                <tbody>
                    <tr v-for="(d,index) in search_result">
                        <th scope="row">{{index+1}}</th>
                        <td><a class="p_title" :href="d.url" target="_blank">{{d.title}}</a></td>
                        <td class="p_desc">{{ d.desc || "" }}</td>
                        <td><a class="oj_name" :href="d.source" target="_blank">{{d.oj}}-{{ d.problem_id }}</a></td>
                        <td>{{ tags_string(d.tags)}}</td>
                        <td>{{ d.date }}</td>
                    </tr>
                </tbody>
            </table>
        </div>
        
        <!-- 代码查看模态框 -->
        <codeShow 
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
    max-width: 1200px;
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

/* 筛选区域样式 */
.filter-section {
    margin-bottom: 2rem;
    border: 1px solid #FFC700;
    background-color: #111;
}

.filter-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 1rem;
    background-color: #332a00;
    cursor: pointer;
    user-select: none;
    border-bottom: 1px solid #FFC700;
}

.filter-header:hover {
    background-color: #443300;
}

.filter-toggle {
    font-size: 0.8rem;
    transition: transform 0.2s ease;
}

.filter-content {
    padding: 1rem;
}

.filter-group {
    margin-bottom: 1.5rem;
}

.filter-group:last-child {
    margin-bottom: 1rem;
}

.filter-group h4 {
    color: #FFD700;
    margin-bottom: 0.75rem;
    font-size: 1.1rem;
    text-transform: uppercase;
    letter-spacing: 0.1em;
}

.checkbox-grid {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(120px, 1fr));
    gap: 0.5rem;
}

.checkbox-label {
    display: flex;
    align-items: center;
    cursor: pointer;
    padding: 0.25rem;
    border-radius: 4px;
    transition: background-color 0.2s ease;
}

.checkbox-label:hover {
    background-color: #221c00;
}

.checkbox-label input[type="checkbox"] {
    margin-right: 0.5rem;
    accent-color: #FFC700;
}

.checkbox-label span {
    color: #FFC700;
    font-size: 0.9rem;
}

.filter-actions {
    text-align: center;
    padding-top: 1rem;
    border-top: 1px dotted #554400;
}

.clear-filters-btn {
    padding: 0.5rem 1rem;
    background-color: transparent;
    color: #FFC700;
    border: 1px solid #FFC700;
    border-radius: 4px;
    cursor: pointer;
    text-transform: uppercase;
    font-size: 0.9rem;
    transition: all 0.2s ease;
}

.clear-filters-btn:hover {
    background-color: #FFC700;
    color: #000;
    box-shadow: 0 0 8px #FFC700;
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

.p_title {
    color: #FFD700;
    text-decoration: none;
    font-weight: bold;
}
.p_title:hover {
    text-decoration: underline;
    color: #fff;
}

.p_desc {
    color: #ccc; /* Lighter gray for description */
    font-size: 0.9em;
}

.oj_name {
    display: inline-block;
    padding: 0.25rem 0.5rem;
    font-size: 0.85rem;
    border-radius: 4px;
    border: 1px solid #FFC700;
    cursor: pointer;
    color: #FFC700;
    background-color: transparent;
    text-transform: uppercase;
    transition: all 0.2s ease-in-out;
    text-decoration: none;
    text-align: center;
    min-width: 80px;
}

.oj_name:hover {
    background-color: #FFC700;
    color: #000;
    box-shadow: 0 0 8px #FFC700;
}
</style>
