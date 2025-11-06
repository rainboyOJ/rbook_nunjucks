## tab fence

来自 [markdown-it-codetabs](https://github.com/cncws/markdown-it-codetabs/blob/main/README.md)


~~~md
```js [g1:JavaScript]
console.log("hello");
```

```py [g1:Python3]
print("hello")
```
~~~

## 伪代码 markdown-it-pseudocodejs

- 来自: https://github.com/samuelhorwitz/markdown-it-pseudocodejs
- 语法文档 : https://github.com/SaswatPadhi/pseudocode.js
- 我写的在线预览工具:https://pseudocode.roj.ac.cn/


使用
```
::: pseudocode
\begin{algorithm}
\caption{Hello World}
\begin{algorithmic}
\FUNCTION{HelloWorld}{}
		\RETURN \CALL{Print}{``Hello, world''}
\ENDFUNCTION
\end{algorithmic}
\end{algorithm}
::: 
```