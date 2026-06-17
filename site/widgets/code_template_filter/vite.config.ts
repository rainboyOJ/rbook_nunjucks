import { defineConfig } from 'vite'
import {resolve} from 'path'
import load_data from "./load_data.js";
import { distDir } from '@rbook/core/paths';
import vue from '@vitejs/plugin-vue'

const root = resolve(new URL('.', import.meta.url).pathname)

export default defineConfig({
    root,
    plugins: [ vue(), load_data()],
    build: {
        outDir: resolve(distDir, 'code_template'),
        emptyOutDir: true,
    },
})
