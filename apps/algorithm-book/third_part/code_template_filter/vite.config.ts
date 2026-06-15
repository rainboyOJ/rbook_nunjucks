import { defineConfig } from 'vite'
// import vue from '@vitejs/plugin-vue'
import {resolve} from 'path'
import load_data from "./load_data.js";
import { distDir } from '@rbook/core/paths';
// import {ViteEjsPlugin} from "vite-plugin-ejs";
import vue from '@vitejs/plugin-vue'

const root = resolve(new URL('.', import.meta.url).pathname)

// https://vitejs.dev/config/
// let p = resolve(new URL('.', import.meta.url).pathname,'src')
// console.log(p)
export default defineConfig({
    root,
    plugins: [ vue(), load_data()],
    // root: resolve(new URL('.', import.meta.url).pathname,'src'),
    build: {
        outDir: resolve(distDir, 'code_template'),
        emptyOutDir: true,
    },
})
