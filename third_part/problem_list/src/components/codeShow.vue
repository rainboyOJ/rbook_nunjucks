<template>
  <div v-if="isVisible" class="modal-overlay" @click="closeModal">
    <div class="modal-container" @click.stop>
      <div class="modal-header">
        <h3>{{ title }}</h3>
        <button class="close-btn" @click="closeModal">&times;</button>
      </div>
      <div class="modal-body">
        <div class="code-actions">
          <button @click="copyCode" class="action-btn">复制代码</button>
          <button @click="downloadCode" class="action-btn">下载</button>
        </div>
        <div class="code-container line-numbers">
          <pre :class="[languageClass, 'line-numbers']"><code :class="languageClass">{{ code }}</code></pre>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup>
import { ref, computed, watch, nextTick } from 'vue'
import { useToast } from "vue-toastification"

const props = defineProps({
  isVisible: {
    type: Boolean,
    default: false
  },
  title: {
    type: String,
    default: '代码查看'
  },
  code: {
    type: String,
    default: ''
  },
  filename: {
    type: String,
    default: ''
  }
})

const emit = defineEmits(['close'])

const toast = useToast()

const languageClass = computed(() => {
  if (!props.filename) return 'language-text'
  
  const ext = props.filename.split('.').pop().toLowerCase()
  const languageMap = {
    'js': 'language-javascript',
    'ts': 'language-typescript',
    'vue': 'language-vue',
    'py': 'language-python',
    'cpp': 'language-clike',
    'c': 'language-clike',
    'java': 'language-java',
    'html': 'language-html',
    'css': 'language-css',
    'scss': 'language-scss',
    'json': 'language-json',
    'md': 'language-markdown',
    'sh': 'language-bash',
    'go': 'language-go',
    'rs': 'language-rust',
    'php': 'language-php',
    'rb': 'language-ruby',
    'sql': 'language-sql'
  }
  
  return languageMap[ext] || 'language-text'
})

const closeModal = () => {
  emit('close')
}

const copyCode = async () => {
  try {
    if (navigator.clipboard && window.isSecureContext) {
      await navigator.clipboard.writeText(props.code)
      toast.success('代码已复制到剪贴板!')
    } else {
      // 降级方案
      const textArea = document.createElement('textarea')
      textArea.value = props.code
      document.body.appendChild(textArea)
      textArea.select()
      document.execCommand('copy')
      document.body.removeChild(textArea)
      toast.success('代码已复制到剪贴板!')
    }
  } catch (err) {
    toast.error('复制失败: ' + err.message)
  }
}

const downloadCode = () => {
  const blob = new Blob([props.code], { type: 'text/plain' })
  const url = URL.createObjectURL(blob)
  const link = document.createElement('a')
  link.href = url
  link.download = props.filename || 'code.txt'
  document.body.appendChild(link)
  link.click()
  document.body.removeChild(link)
  URL.revokeObjectURL(url)
  toast.success(`开始下载 ${props.filename || 'code.txt'}`)
}

// 监听代码变化，重新高亮
watch(() => props.code, async () => {
  if (props.isVisible && props.code) {
    await nextTick()
    highlightCode()
  }
})

watch(() => props.isVisible, async (newValue) => {
  if (newValue && props.code) {
    await nextTick()
    highlightCode()
  }
})

const highlightCode = () => {
  // 使用 Prism.js 进行代码高亮
  if (window.Prism) {
    // 延迟执行确保 DOM 更新完成
    setTimeout(() => {
      window.Prism.highlightAll()
    }, 100)
  }
}
</script>

<style scoped>
.modal-overlay {
  position: fixed;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background-color: rgba(0, 0, 0, 0.8);
  display: flex;
  justify-content: center;
  align-items: center;
  z-index: 1000;
  backdrop-filter: blur(4px);
}

.modal-container {
  background-color: #0a0a0a;
  border: 2px solid #FFC700;
  border-radius: 8px;
  max-width: 90vw;
  max-height: 90vh;
  width: 1000px;
  box-shadow: 0 0 30px rgba(255, 199, 0, 0.3);
  display: flex;
  flex-direction: column;
  overflow: hidden;
}

.modal-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 1rem 1.5rem;
  background-color: #1a1a1a;
  border-bottom: 1px solid #FFC700;
}

.modal-header h3 {
  color: #FFD700;
  margin: 0;
  font-size: 1.25rem;
  font-weight: bold;
  text-shadow: 0 0 5px #FFC700;
}

.close-btn {
  background: none;
  border: none;
  color: #FFC700;
  font-size: 2rem;
  cursor: pointer;
  padding: 0;
  width: 30px;
  height: 30px;
  display: flex;
  align-items: center;
  justify-content: center;
  border-radius: 4px;
  transition: all 0.2s ease;
}

.close-btn:hover {
  background-color: #FFC700;
  color: #000;
  box-shadow: 0 0 10px #FFC700;
}

.modal-body {
  padding: 0;
  display: flex;
  flex-direction: column;
  overflow: hidden;
}

.code-actions {
  padding: 1rem 1.5rem;
  background-color: #1a1a1a;
  border-bottom: 1px solid #333;
  display: flex;
  gap: 0.5rem;
}

.action-btn {
  padding: 0.5rem 1rem;
  background-color: #111;
  color: #FFC700;
  border: 1px solid #FFC700;
  border-radius: 4px;
  cursor: pointer;
  font-size: 0.875rem;
  text-transform: uppercase;
  transition: all 0.2s ease;
}

.action-btn:hover {
  background-color: #FFC700;
  color: #000;
  box-shadow: 0 0 8px #FFC700;
}

.code-container {
  overflow: auto;
  padding: 1rem;
}

.code-container pre {
  border: 1px solid #333;
  border-radius: 4px;
  /* 移除 overflow-x: auto，让滚动条在父容器上显示 */
  white-space: pre;
  word-wrap: normal;
  /* overflow-x: hidden; */
  /* overflow-y: hidden; */
}

/* Prism.js 主题样式覆盖 */
:deep(.token.comment),
:deep(.token.prolog),
:deep(.token.doctype),
:deep(.token.cdata) {
  color: #666;
  font-style: italic;
}

:deep(.token.punctuation) {
  color: #FFC700;
}

:deep(.token.property),
:deep(.token.tag),
:deep(.token.boolean),
:deep(.token.number),
:deep(.token.constant),
:deep(.token.symbol),
:deep(.token.deleted) {
  color: #FF6B6B;
}

:deep(.token.selector),
:deep(.token.attr-name),
:deep(.token.string),
:deep(.token.char),
:deep(.token.builtin),
:deep(.token.inserted) {
  color: #4ECDC4;
}

:deep(.token.operator),
:deep(.token.entity),
:deep(.token.url),
:deep(.language-css .token.string),
:deep(.style .token.string) {
  color: #FFD93D;
}

:deep(.token.atrule),
:deep(.token.attr-value),
:deep(.token.keyword) {
  color: #FF6B6B;
  font-weight: bold;
}

:deep(.token.function) {
  color: #95E77E;
}

:deep(.token.regex),
:deep(.token.important),
:deep(.token.variable) {
  color: #FFB6C1;
}



/* 滚动条样式 */
.code-container::-webkit-scrollbar {
  width: 8px;
  height: 8px;
}
:deep(pre[class*="language-"]) {
  overflow: unset;
  padding-bottom: 1.5rem; /* 增加底部内边距，确保最后一行不被遮挡 */
  margin-bottom: 0.5rem; /* 添加底部外边距，确保内容不被遮挡 */
}
 

.code-container::-webkit-scrollbar-track {
  background: #1a1a1a;
}

.code-container::-webkit-scrollbar-thumb {
  background: #FFC700;
  border-radius: 4px;
}

.code-container::-webkit-scrollbar-thumb:hover {
  background: #FFD700;
}
</style>
