import { createApp } from 'vue'
import type { Plugin } from 'vue'
import Toast from "vue-toastification";
import './style.scss'
import "vue-toastification/dist/index.css";
import App from './App.vue'

const app = createApp(App)

const toast_options = {
    // You can set your default options here
};
app.use(Toast as unknown as Plugin, toast_options);

app.provide('template_array',template_array)
app.mount('#app')
