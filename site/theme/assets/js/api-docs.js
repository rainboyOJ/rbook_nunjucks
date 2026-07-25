document.addEventListener('click', async (event) => {
  const copyButton = event.target.closest('.zeroclipboard-container');
  if (!copyButton) return;

  const code = copyButton.parentElement?.querySelector('pre code');
  if (!code) return;

  try {
    await navigator.clipboard.writeText(code.textContent || '');
    copyButton.classList.add('copied');
    setTimeout(() => copyButton.classList.remove('copied'), 1500);
  } catch (error) {
    console.error('复制代码失败', error);
  }
});
