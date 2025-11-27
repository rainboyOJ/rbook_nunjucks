/**
 * @file Vue 应用程序的主入口文件 (main.js)。
 * @description
 * 该文件负责初始化 Vue 应用、注册插件、并挂载根组件。
 * 
 * 主要流程：
 * 1. 导入 Vue 核心库、根组件 App、全局样式文件以及第三方插件（如 vue-toastification 和 bootstrap）。
 * 2. 使用 `createApp(App)` 创建 Vue 应用实例。
 * 3. 初始化并注册 `vue-toastification` 插件，用于全局的消息提示。
 * 4. 通过 `app.provide` 将一个名为 `template_array` 的全局变量注入到应用中，
 *    使得所有子组件都可以通过 `inject` 来访问这个数据。这个变量预计由外部环境（如模板引擎或直接在 HTML 中定义的脚本）提供。
 * 5. 调用 `app.mount('#app')` 将整个 Vue 应用挂载到 HTML 页面中 ID 为 `app` 的 DOM 元素上，从而启动应用。
 */
import { createApp ,provide} from 'vue'
import Toast from "vue-toastification";
import './style.scss'
import "vue-toastification/dist/index.css";
import App from './App.vue'

const app = createApp(App)

// console.log(template_array)

const toast_options = {
    // You can set your default options here
};
app.use(Toast, toast_options);


//
app.provide('template_array',template_array)
app.mount('#app')
