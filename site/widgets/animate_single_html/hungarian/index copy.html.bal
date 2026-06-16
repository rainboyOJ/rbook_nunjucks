<!DOCTYPE html>
<html lang="zh-CN">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>匈牙利算法可视化 - 递归深度解析</title>
    <!-- Vue 3 -->
    <script src="https://unpkg.com/vue@3/dist/vue.global.js"></script>
    <!-- Cytoscape.js -->
    <script src="https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.26.0/cytoscape.min.js"></script>
    <!-- Tailwind CSS -->
    <script src="https://cdn.tailwindcss.com"></script>
    <style>
        body { font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; }
        #cy { width: 100%; height: 500px; background-color: #f8fafc; border: 1px solid #e2e8f0; border-radius: 0.5rem; }
        .log-item { transition: all 0.3s ease; }
        .log-enter-active, .log-leave-active { transition: all 0.5s ease; }
        .log-enter-from, .log-leave-to { opacity: 0; transform: translateX(20px); }
        
        /* Custom Scrollbar */
        ::-webkit-scrollbar { width: 8px; }
        ::-webkit-scrollbar-track { background: #f1f1f1; }
        ::-webkit-scrollbar-thumb { background: #cbd5e1; border-radius: 4px; }
        ::-webkit-scrollbar-thumb:hover { background: #94a3b8; }
    </style>
</head>
<body class="bg-slate-100 text-slate-800 h-screen flex flex-col overflow-hidden">

<div id="app" class="flex flex-col h-full">

    <!-- Header -->
    <header class="bg-white shadow-sm p-4 z-10">
        <div class="container mx-auto flex justify-between items-center">
            <h1 class="text-xl font-bold text-indigo-600 flex items-center gap-2">
                <svg xmlns="http://www.w3.org/2000/svg" class="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 10V3L4 14h7v7l9-11h-7z" /></svg>
                匈牙利算法可视化 (DFS版)
            </h1>
            <div class="text-sm text-gray-500">
                蓝色: 左部点(u) | 橙色: 右部点(v) | 红色连线: 匹配 | 绿色连线: 尝试中
            </div>
        </div>
    </header>

    <!-- Main Content -->
    <main class="flex-1 flex overflow-hidden">
        
        <!-- Left Panel: Controls & Data -->
        <div class="w-1/4 bg-white border-r border-gray-200 p-4 overflow-y-auto flex flex-col gap-4">
            
            <div class="space-y-2">
                <h2 class="font-semibold text-gray-700">1. 数据设置</h2>
                <div class="grid grid-cols-2 gap-2">
                    <div>
                        <label class="block text-xs font-medium text-gray-500">左边点数 (N)</label>
                        <input type="number" v-model.number="n" min="1" max="8" class="w-full border rounded px-2 py-1 text-sm">
                    </div>
                    <div>
                        <label class="block text-xs font-medium text-gray-500">右边点数 (M)</label>
                        <input type="number" v-model.number="m" min="1" max="8" class="w-full border rounded px-2 py-1 text-sm">
                    </div>
                </div>
                <div>
                    <label class="block text-xs font-medium text-gray-500">边 (格式: u v，每行一条)</label>
                    <textarea v-model="edgeInput" rows="5" class="w-full border rounded px-2 py-1 text-sm font-mono" placeholder="1 1&#10;1 2&#10;2 1"></textarea>
                </div>
                <div class="flex gap-2">
                    <button @click="generateRandom" class="flex-1 bg-gray-200 hover:bg-gray-300 text-gray-700 py-1 px-3 rounded text-sm transition">随机生成</button>
                    <button @click="applyData" class="flex-1 bg-indigo-100 hover:bg-indigo-200 text-indigo-700 py-1 px-3 rounded text-sm transition">应用数据</button>
                </div>
            </div>

            <div class="border-t pt-4 space-y-2">
                <h2 class="font-semibold text-gray-700">2. 算法控制</h2>
                <div class="flex flex-col gap-2">
                    <button v-if="!isRunning" @click="startAlgorithm" class="w-full bg-indigo-600 hover:bg-indigo-700 text-white font-bold py-2 px-4 rounded shadow transition">
                        开始算法
                    </button>
                    
                    <div v-else class="flex gap-2">
                        <button @click="nextStep" :disabled="!isWaiting" :class="{'opacity-50 cursor-not-allowed': !isWaiting}" class="flex-1 bg-green-600 hover:bg-green-700 text-white font-bold py-2 px-4 rounded shadow transition flex items-center justify-center gap-1">
                            <span>下一步</span>
                            <svg xmlns="http://www.w3.org/2000/svg" class="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 5l7 7-7 7M5 5l7 7-7 7" /></svg>
                        </button>
                        <button @click="resetAlgorithm" class="bg-red-100 hover:bg-red-200 text-red-700 font-bold py-2 px-3 rounded transition">重置</button>
                    </div>

                    <div v-if="isRunning" class="text-center p-2 bg-yellow-50 border border-yellow-200 rounded text-sm text-yellow-800">
                        <span v-if="isWaiting" class="font-bold animate-pulse">👉 等待下一步...</span>
                        <span v-else>计算中...</span>
                    </div>
                </div>
            </div>

            <!-- Status Monitor -->
            <div class="border-t pt-4 flex-1">
                <h2 class="font-semibold text-gray-700 mb-2">3. 状态监控</h2>
                <div class="space-y-3">
                    <div>
                        <div class="text-xs font-bold text-gray-500 uppercase">Match 数组 (右边点谁占了)</div>
                        <div class="flex flex-wrap gap-1 mt-1">
                            <div v-for="i in m" :key="'m'+i" class="flex flex-col items-center border rounded p-1 min-w-[30px]" :class="match[i] ? 'bg-red-50 border-red-200' : 'bg-gray-50'">
                                <span class="text-[10px] text-gray-400">v{{i}}</span>
                                <span class="font-bold" :class="match[i] ? 'text-red-600' : 'text-gray-300'">{{ match[i] || '-' }}</span>
                            </div>
                        </div>
                    </div>
                    <div>
                        <div class="text-xs font-bold text-gray-500 uppercase">Vis 数组 (本轮是否访问)</div>
                        <div class="flex flex-wrap gap-1 mt-1">
                            <div v-for="i in m" :key="'v'+i" class="flex flex-col items-center border rounded p-1 min-w-[30px] transition-colors duration-300" :class="vis[i] ? 'bg-orange-100 border-orange-300' : 'bg-gray-50'">
                                <span class="text-[10px] text-gray-400">v{{i}}</span>
                                <span class="font-bold text-xs" :class="vis[i] ? 'text-orange-600' : 'text-gray-300'">{{ vis[i] ? 'T' : 'F' }}</span>
                            </div>
                        </div>
                    </div>
                     <div class="mt-2 p-2 bg-green-50 rounded border border-green-100">
                        <span class="text-sm font-bold text-green-800">当前最大匹配数: {{ totalMatched }}</span>
                    </div>
                </div>
            </div>
        </div>

        <!-- Center: Graph -->
        <div class="flex-1 bg-slate-50 relative p-4 flex flex-col">
            <div id="cy" class="flex-1 shadow-inner bg-white"></div>
            <!-- Legend overlay -->
            <div class="absolute top-6 right-6 bg-white/90 p-2 rounded shadow text-xs space-y-1 border">
                <div class="flex items-center gap-2"><div class="w-3 h-3 rounded-full bg-blue-500"></div> 左部点 (u)</div>
                <div class="flex items-center gap-2"><div class="w-3 h-3 rounded-full bg-orange-500"></div> 右部点 (v)</div>
                <div class="flex items-center gap-2"><div class="w-8 h-1 bg-gray-300"></div> 未匹配边</div>
                <div class="flex items-center gap-2"><div class="w-8 h-1 bg-red-500"></div> 匹配边 (Match)</div>
                <div class="flex items-center gap-2"><div class="w-8 h-1 bg-green-500 dashed border-b-2 border-dashed"></div> 正在DFS尝试</div>
            </div>
        </div>

        <!-- Right Panel: Logs -->
        <div class="w-1/4 bg-white border-l border-gray-200 flex flex-col h-full">
            <div class="p-3 border-b bg-gray-50 flex justify-between items-center">
                <h2 class="font-semibold text-gray-700">执行日志</h2>
                <button @click="logs = []" class="text-xs text-gray-400 hover:text-gray-600">清空</button>
            </div>
            <div id="log-container" class="flex-1 overflow-y-auto p-4 space-y-3 bg-slate-50">
                <div v-if="logs.length === 0" class="text-center text-gray-400 text-sm mt-10">
                    等待开始...<br>请点击左侧“开始算法”
                </div>
                <transition-group name="log">
                    <div v-for="(log, index) in logs" :key="index" class="log-item p-3 rounded-lg border text-sm shadow-sm relative pl-4" 
                        :class="{
                            'bg-blue-50 border-blue-200 text-blue-800': log.type === 'info',
                            'bg-green-50 border-green-200 text-green-800': log.type === 'success',
                            'bg-red-50 border-red-200 text-red-800': log.type === 'fail',
                            'bg-yellow-50 border-yellow-200 text-yellow-800': log.type === 'warning',
                            'bg-purple-50 border-purple-200 text-purple-800': log.type === 'dfs',
                            'ml-0': log.depth === 0,
                            'ml-4': log.depth === 1,
                            'ml-8': log.depth === 2,
                            'ml-12': log.depth >= 3
                        }">
                        <!-- Connector line for recursion depth -->
                        <div v-if="log.depth > 0" class="absolute left-[-10px] top-1/2 w-3 h-[1px] bg-gray-300"></div>
                        
                        <div class="font-bold mb-1 flex justify-between">
                            <span>{{ log.title }}</span>
                            <span class="text-xs opacity-60 font-mono" v-if="log.depth > 0">Depth: {{log.depth}}</span>
                        </div>
                        <div v-html="log.msg"></div>
                    </div>
                </transition-group>
            </div>
        </div>

    </main>
</div>

<script>
const { createApp, ref, onMounted, nextTick, computed } = Vue;

createApp({
    setup() {
        // Data Models
        const n = ref(4);
        const m = ref(4);
        const edgeInput = ref("1 1\n1 2\n2 1\n2 3\n3 2\n3 4\n4 1");
        
        // Algorithm State
        const adj = ref({});
        const match = ref({}); // Right -> Left
        const vis = ref({});   // Right visited status
        const logs = ref([]);
        const totalMatched = ref(0);
        
        // Control Flow
        const isRunning = ref(false);
        const isWaiting = ref(false);
        let stepResolver = null; // Promise resolver for pausing
        
        // Cytoscape Instance
        let cy = null;

        // --- Visualization Logic ---
        const initGraph = () => {
            if(cy) cy.destroy();
            
            const elements = [];
            // Nodes
            for(let i=1; i<=n.value; i++) {
                elements.push({ data: { id: `u${i}`, label: `u${i}`, type: 'left' }, position: { x: 100, y: i * 80 } });
            }
            for(let i=1; i<=m.value; i++) {
                elements.push({ data: { id: `v${i}`, label: `v${i}`, type: 'right' }, position: { x: 400, y: i * 80 } });
            }
            // Edges
            const lines = edgeInput.value.trim().split('\n');
            adj.value = {};
            // Init adj
            for(let i=1; i<=n.value; i++) adj.value[i] = [];

            lines.forEach(line => {
                const parts = line.trim().split(/\s+/);
                if(parts.length >= 2) {
                    const u = parseInt(parts[0]);
                    const v = parseInt(parts[1]);
                    if(u && v && u <= n.value && v <= m.value) {
                        elements.push({ data: { id: `e${u}-${v}`, source: `u${u}`, target: `v${v}` } });
                        if(!adj.value[u]) adj.value[u] = [];
                        if(!adj.value[u].includes(v)) adj.value[u].push(v);
                    }
                }
            });

            cy = cytoscape({
                container: document.getElementById('cy'),
                elements: elements,
                style: [
                    {
                        selector: 'node',
                        style: {
                            'background-color': '#94a3b8',
                            'label': 'data(label)',
                            'color': '#fff',
                            'text-valign': 'center',
                            'text-halign': 'center',
                            'width': 40, 'height': 40,
                            'font-size': 14,
                            'font-weight': 'bold'
                        }
                    },
                    { selector: 'node[type="left"]', style: { 'background-color': '#3b82f6' } }, // Blue
                    { selector: 'node[type="right"]', style: { 'background-color': '#f97316' } }, // Orange
                    {
                        selector: 'edge',
                        style: {
                            'width': 2,
                            'line-color': '#cbd5e1', // Slate 300
                            'curve-style': 'bezier',
                            'target-arrow-shape': 'triangle',
                            'target-arrow-color': '#cbd5e1'
                        }
                    },
                    // Dynamic Styles
                    { selector: '.highlight-node', style: { 'border-width': 4, 'border-color': '#8b5cf6', 'width': 50, 'height': 50 } }, // Purple border
                    { selector: '.vis-node', style: { 'background-color': '#fbbf24', 'color': '#000' } }, // Yellow
                    { selector: '.matched-edge', style: { 'line-color': '#ef4444', 'width': 4, 'target-arrow-color': '#ef4444' } }, // Red
                    { selector: '.scanning-edge', style: { 'line-color': '#22c55e', 'width': 4, 'line-style': 'dashed', 'target-arrow-color': '#22c55e' } } // Green
                ],
                layout: { name: 'preset' },
                userZoomingEnabled: true,
                userPanningEnabled: true,
                boxSelectionEnabled: false
            });
            
            cy.fit(20);
        };

        const updateGraphVisuals = (u, v, type) => {
            // type: 'scan', 'match', 'reset-scan'
            if(type === 'scan') {
                cy.$(`edge[source="u${u}"][target="v${v}"]`).addClass('scanning-edge');
                cy.$(`#u${u}`).addClass('highlight-node');
                cy.$(`#v${v}`).addClass('highlight-node');
            } else if (type === 'match') {
                // Remove scanning style
                cy.$(`edge[source="u${u}"][target="v${v}"]`).removeClass('scanning-edge');
                // Add matched style
                cy.$(`edge[source="u${u}"][target="v${v}"]`).addClass('matched-edge');
            } else if (type === 'unmatch') {
                 cy.$(`edge[target="v${v}"]`).removeClass('matched-edge');
            } else if (type === 'clear-vis') {
                 cy.nodes().removeClass('vis-node highlight-node');
                 cy.edges().removeClass('scanning-edge');
            } else if (type === 'visit') {
                 cy.$(`#v${v}`).addClass('vis-node');
            }
        };

        // --- Logging Logic ---
        const addLog = (title, msg, type = 'info', depth = 0) => {
            logs.value.push({ title, msg, type, depth });
            // Auto scroll log
            nextTick(() => {
                const container = document.getElementById('log-container');
                if(container) container.scrollTop = container.scrollHeight;
            });
        };

        // --- Algorithm Control ---
        const waitForStep = async () => {
            isWaiting.value = true;
            return new Promise(resolve => {
                stepResolver = resolve;
            });
        };

        const nextStep = () => {
            if (stepResolver) {
                isWaiting.value = false;
                stepResolver();
                stepResolver = null;
            }
        };

        // --- DFS Algorithm (Async for Visuals) ---
        const dfs = async (u, depth) => {
            addLog(`DFS(u${u}) 开始`, `左边点 <b>u${u}</b> 正在尝试找对象。`, 'dfs', depth);
            
            // Highlight current U
            cy.$(`#u${u}`).addClass('highlight-node');

            const neighbors = adj.value[u] || [];
            
            for (const v of neighbors) {
                // Visual: Highlight Scanning Edge
                updateGraphVisuals(u, v, 'scan');
                addLog(`尝试 u${u} -> v${v}`, `检查邻居 <b>v${v}</b>`, 'info', depth);
                
                await waitForStep(); // PAUSE 1: Before checking logic

                // Logic: Check Vis
                if (vis.value[v]) {
                    addLog(`跳过 v${v}`, `<b>vis[${v}] == true</b>。<br>v${v} 在这一轮已经被问过了，防止死循环。`, 'warning', depth);
                    cy.$(`edge[source="u${u}"][target="v${v}"]`).removeClass('scanning-edge');
                    continue;
                }

                // Mark Vis
                vis.value[v] = true;
                updateGraphVisuals(0, v, 'visit'); // Highlight V as visited
                
                const currentOwner = match.value[v];

                if (!currentOwner) {
                    // Case 1: Empty
                    addLog(`配对成功!`, `<b>v${v}</b> 是空的 (match[${v}] == 0)。<br>u${u} 直接拿下!`, 'success', depth);
                    
                    match.value[v] = u;
                    updateGraphVisuals(u, v, 'match');
                    await waitForStep(); // PAUSE 2: Success confirm
                    
                    cy.$(`#u${u}`).removeClass('highlight-node'); // remove highlight
                    return true;

                } else {
                    // Case 2: Occupied, try negotiation
                    addLog(`冲突!`, `<b>v${v}</b> 已经被 <b>u${currentOwner}</b> 占了。<br>触发协商：尝试让 <b>u${currentOwner}</b> 挪一挪 (递归调用)。`, 'warning', depth);
                    
                    await waitForStep(); // PAUSE 3: Before Recursion

                    // Visual: un-highlight current scanning edge temporarily to focus on recursion
                    // cy.$(`edge[source="u${u}"][target="v${v}"]`).removeClass('scanning-edge');

                    if (await dfs(currentOwner, depth + 1)) {
                        addLog(`协商成功!`, `<b>u${currentOwner}</b> 成功找到了新位置。<br><b>v${v}</b> 现在空出来了，归 <b>u${u}</b> 所有。`, 'success', depth);
                        
                        // Update visual match: Remove old match line
                        updateGraphVisuals(0, v, 'unmatch'); // Clear old red line
                        match.value[v] = u;
                        updateGraphVisuals(u, v, 'match');   // Add new red line
                        
                        await waitForStep(); // PAUSE 4: Swap confirm
                        cy.$(`#u${u}`).removeClass('highlight-node');
                        return true;
                    } else {
                        addLog(`协商失败`, `<b>u${currentOwner}</b> 没地方挪。<br>u${u} 无法在这个分支找到位置。`, 'fail', depth);
                         cy.$(`edge[source="u${u}"][target="v${v}"]`).removeClass('scanning-edge');
                    }
                }
            }

            addLog(`DFS(u${u}) 结束`, `u${u} 遍历了所有邻居，均未找到可行方案。`, 'fail', depth);
            cy.$(`#u${u}`).removeClass('highlight-node');
            return false;
        };

        const startAlgorithm = async () => {
            if(isRunning.value) return;
            isRunning.value = true;
            match.value = {}; // Reset Match
            vis.value = {};
            totalMatched.value = 0;
            logs.value = [];
            
            // Clear graph styles
            cy.elements().removeClass('matched-edge vis-node highlight-node scanning-edge');

            addLog('算法开始', '准备遍历左边的每个点 i = 1 to N', 'info', 0);

            for (let i = 1; i <= n.value; i++) {
                // Reset Vis for each round
                vis.value = {};
                for(let k=1; k<=m.value; k++) vis.value[k] = false;
                
                // Visual clear vis styles
                updateGraphVisuals(0, 0, 'clear-vis');

                addLog(`ROUND ${i}`, `开始为左边点 <b>u${i}</b> 找对象。<br>清空 vis 数组。`, 'info', 0);
                
                await waitForStep(); // PAUSE Start of Round

                if (await dfs(i, 0)) {
                    totalMatched.value++;
                    addLog(`ROUND ${i} 结束`, `u${i} 匹配成功！当前总匹配数: ${totalMatched.value}`, 'success', 0);
                } else {
                    addLog(`ROUND ${i} 结束`, `u${i} 匹配失败。`, 'fail', 0);
                }
            }

            addLog('算法完成', `最终最大匹配数: ${totalMatched.value}`, 'success', 0);
            isRunning.value = false;
            isWaiting.value = false;
        };

        const resetAlgorithm = () => {
            isRunning.value = false;
            isWaiting.value = false;
            if(stepResolver) stepResolver(); // Unlock any pending promise
            stepResolver = null;
            applyData(); // Re-draw graph
        };

        // --- Utils ---
        const generateRandom = () => {
            const newN = Math.floor(Math.random() * 3) + 3; // 3-5
            const newM = Math.floor(Math.random() * 3) + 3; // 3-5
            n.value = newN;
            m.value = newM;
            
            let edges = "";
            for(let i=1; i<=newN; i++) {
                // Ensure at least one edge usually
                const count = Math.floor(Math.random() * 2) + 1; 
                const targets = new Set();
                while(targets.size < count) {
                    targets.add(Math.floor(Math.random() * newM) + 1);
                }
                targets.forEach(t => edges += `${i} ${t}\n`);
            }
            edgeInput.value = edges.trim();
            applyData();
        };

        const applyData = () => {
            match.value = {};
            for(let k=1; k<=m.value; k++) {
                match.value[k] = 0;
                vis.value[k] = false;
            }
            logs.value = [];
            totalMatched.value = 0;
            initGraph();
        };

        onMounted(() => {
            applyData();
        });

        return {
            n, m, edgeInput,
            logs, match, vis, totalMatched,
            isRunning, isWaiting,
            generateRandom, applyData, startAlgorithm, nextStep, resetAlgorithm
        };
    }
});
</script>
</body>
</html>
<!-- ```

### 这个工具如何帮助你理解？

1.  **观察 `vis` 的“封条”效应**：
    * 在复杂的递归中（比如我之前给你的“多米诺骨牌”例子），当你点击“下一步”时，注意观察 `vis` 数组面板。
    * 你会看到 `vis` 变成 `True`（黄色高亮），并且日志会明确提示：`“跳过 v1，因为 vis[1] == true”`。这就是防止死循环的现场直播。

2.  **观察递归栈（Depth）**：
    * 右侧日志有缩进（Depth: 0, 1, 2...）。
    * 你能清晰地看到 `DFS(u1)` 调用了 `DFS(u2)`，然后 `DFS(u2)` 返回成功，`DFS(u1)` 也就跟着成功了。这种**嵌套关系**在静态代码里很难看出来，但在日志里一目了然。

3.  **颜色编码**：
    * **红色粗线**：已经敲定的匹配（Match）。
    * **绿色虚线**：正在尝试的“暧昧”关系。
    * **黄色节点**：这一轮已经被问过的右边点（Vis）。

你可以尝试输入我之前给你的“多米诺骨牌”数据：
```text
N=4, M=4
边数据：
1 1
1 2
2 1
2 3
3 2
3 4
4 1 -->