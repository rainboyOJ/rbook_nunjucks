<script setup>
import {inject,ref,computed} from 'vue'
import { useToast } from "vue-toastification";
  // import HelloWorld from './components/HelloWorld.vue'
// import tableCell from './components/tableCell.vue'
import Fuse from 'fuse.js'

const template_array = inject('template_array')
const search_text = ref("")
const toast = useToast()

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
		"title"
	]
}

const fuse = new Fuse(template_array,FuseOptions)

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
    toast.info('查看功能待实现!');
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
        if( search_text.value.length == 0)
            return template_array
        else
            return fuse.search(search_text.value).map(r => r.item);
    }
)
</script>

<template>
    <div class="container">
      <h1>代码模板过滤器</h1>
        <div class="search_box">
            <div class="input-group flex-nowrap">
                <span class="input-group-text" id="addon-wrapping">搜索</span>
                <!-- <input @keydown.enter="search_func"  v-model="search_text" type="text" class="form-control" placeholder="请输入内容" aria-label="search" aria-describedby="addon-wrapping"> -->
                <input  v-model="search_text" type="text" class="form-control" placeholder="请输入内容" aria-label="search" aria-describedby="addon-wrapping">
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
                        <th scope="col">标签</th>
                        <th scope="col">功能</th>
                    </tr>
                </thead>
                <tbody>
                    <tr v-for="(d,index) in search_result">
                        <th scope="row">{{index+1}}</th>
                        <td>{{d.title}}</td>
                        <td>{{ tags_string(d.tags)}}</td>
                        <td>
                          <div class="btn-group">
                            <button @click="view_code(d)" class="">查看</button>
                            <button @click="download_code(d)" class="">下载</button>
                            <button @click="template_code_copy(d)" class="">复制</button>
                            <button @click="copy_shell(d)" class="" v-show="d.sh">命令</button>
                          </div>
                        </td>
                    </tr>
                </tbody>
            </table>
        </div>
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
