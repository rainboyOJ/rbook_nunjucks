---
title: "LLDB Python 脚本入门"
date: 2025-10-17 08:02
toc: true
tags: ["lldb", "python", "debug"]
categories: ["debug"]
---

[[TOC]]

LLDB Python 脚本入门

## 环境准备

### 检查 LLDB Python 支持

```bash
$ lldb
(lldb) script
>>> import lldb
>>> lldb.debugger
Debugger (instance: "debugger_1", id: 1)
```

### 必备工具

- LLDB
- Python 3.x
- 文本编辑器

## 基础概念

### 核心类

| 类名 | 描述 |
|------|------|
| SBDebugger | 调试器主入口点 |
| SBTarget | 被调试的目标程序 |
| SBProcess | 目标进程 |
| SBThread | 线程 |
| SBFrame | 栈帧 |
| SBBreakpoint | 断点 |

### 获取调试器实例

```python
# 在 LLDB 脚本环境中
debugger = lldb.debugger

# 创建新的调试器实例
debugger = lldb.SBDebugger.Create()
debugger.SetAsync(False)

# 通过当前进程获取
target = debugger.GetSelectedTarget()
process = target.GetProcess()
```

## 快速开始

### 内联执行

```bash
(lldb) script
>>> print("Hello from Python!")
>>> target = lldb.debugger.GetSelectedTarget()
>>> print(f"Target: {target}")
```

### 外部脚本

创建 `my_script.py`：

```python
import lldb

def print_registers(debugger, command, result, internal_dict):
    frame = debugger.GetSelectedTarget().GetProcess().GetSelectedThread().GetSelectedFrame()
    if not frame.IsValid():
        result.AppendMessage("No valid frame selected")
        return
    
    registers = frame.GetRegisters()
    result.AppendMessage("Register values:")
    for regs in registers:
        result.AppendMessage(f"  {regs.GetName()}:")
        for reg in regs:
            result.AppendMessage(f"    {reg.GetName():10} = {reg.GetValue()}")

def __lldb_init_module(debugger, internal_dict):
    debugger.HandleCommand('command script add -f my_script.print_registers preg')
```

加载脚本：

```bash
(lldb) command script import my_script.py
```

## 核心 API

### 目标程序管理

```python
def target_info(debugger, command, result, internal_dict):
    target = debugger.GetSelectedTarget()
    if target.IsValid():
        result.AppendMessage(f"Target: {target}")
        result.AppendMessage(f"Executable: {target.GetExecutable()}")
        result.AppendMessage(f"Modules: {target.GetNumModules()}")
    else:
        result.AppendMessage("No target selected")
```

### 断点管理

```python
def set_conditional_breakpoint(debugger, command, result, internal_dict):
    args = command.split()
    if len(args) < 2:
        result.AppendMessage("Usage: cb <location> <condition>")
        return
    
    location = args[0]
    condition = " ".join(args[1:])
    
    target = debugger.GetSelectedTarget()
    breakpoint = target.BreakpointCreateByLocation(location)
    breakpoint.SetCondition(condition)
    
    result.AppendMessage(f"Breakpoint set at {location} with condition: {condition}")
```

### 内存操作

```python
def examine_memory(debugger, command, result, internal_dict):
    args = command.split()
    if len(args) < 2:
        result.AppendMessage("Usage: xm <address> <count>")
        return
    
    address = int(args[0], 0)
    count = int(args[1])
    
    target = debugger.GetSelectedTarget()
    process = target.GetProcess()
    
    error = lldb.SBError()
    memory = process.ReadMemory(address, count, error)
    
    if error.Success():
        for i in range(0, len(memory), 16):
            hex_str = " ".join(f"{b:02x}" for b in memory[i:i+16])
            result.AppendMessage(f"  0x{address+i:016x}: {hex_str}")
```

## 实用脚本

### 崩溃分析

```python
def analyze_crash(debugger, command, result, internal_dict):
    target = debugger.GetSelectedTarget()
    process = target.GetProcess()
    
    if process.GetState() == lldb.eStateStopped:
        thread = process.GetSelectedThread()
        frame = thread.GetSelectedFrame()
        
        result.AppendMessage("=== Crash Analysis ===")
        result.AppendMessage(f"Stop Reason: {thread.GetStopReason()}")
        
        # 寄存器
        result.AppendMessage("\nRegisters:")
        registers = frame.GetRegisters()
        for regs in registers:
            for reg in regs:
                if reg.GetValue():
                    result.AppendMessage(f"  {reg.GetName()}: {reg.GetValue()}")
        
        # 局部变量
        result.AppendMessage("\nLocal variables:")
        variables = frame.GetVariables(True, True, True, True)
        for var in variables:
            result.AppendMessage(f"  {var.GetName()}: {var.GetValue()}")
        
        # 栈回溯
        result.AppendMessage("\nBacktrace:")
        for i in range(thread.GetNumFrames()):
            frame = thread.GetFrameAtIndex(i)
            result.AppendMessage(f"  #{i}: {frame}")
```

### 性能分析

```python
import time

class PerformanceProfiler:
    def __init__(self):
        self.breakpoint_times = {}
        self.start_time = None
    
    def start_profiling(self, debugger, command, result, internal_dict):
        self.start_time = time.time()
        self.breakpoint_times = {}
        
        target = debugger.GetSelectedTarget()
        functions = ["malloc", "free", "pthread_create"]
        
        for func in functions:
            bp = target.BreakpointCreateByName(func)
            bp.SetScriptCallbackFunction("performance_profiler.breakpoint_hit")
        
        result.AppendMessage("Profiling started")
    
    def breakpoint_hit(self, frame, bp_loc, dict):
        current_time = time.time() - self.start_time
        func_name = frame.GetFunctionName()
        
        if func_name not in self.breakpoint_times:
            self.breakpoint_times[func_name] = []
        self.breakpoint_times[func_name].append(current_time)
        return False
    
    def show_results(self, debugger, command, result, internal_dict):
        result.AppendMessage("=== Performance Profile ===")
        for func, times in self.breakpoint_times.items():
            result.AppendMessage(f"{func}: {len(times)} calls")

profiler = PerformanceProfiler()

def start_profiling(debugger, command, result, internal_dict):
    profiler.start_profiling(debugger, command, result, internal_dict)

def show_results(debugger, command, result, internal_dict):
    profiler.show_results(debugger, command, result, internal_dict)

def breakpoint_hit(frame, bp_loc, dict):
    return profiler.breakpoint_hit(frame, bp_loc, dict)
```

### 数据结构可视化

```python
def visualize_linked_list(debugger, command, result, internal_dict):
    frame = debugger.GetSelectedTarget().GetProcess().GetSelectedThread().GetSelectedFrame()
    
    head_ptr = frame.FindVariable("list_head")
    if not head_ptr.IsValid():
        result.AppendMessage("List head not found")
        return
    
    result.AppendMessage("Linked List:")
    current = head_ptr
    count = 0
    max_nodes = 20
    
    while current.IsValid() and count < max_nodes:
        data = current.GetChildMemberWithName("data")
        next_ptr = current.GetChildMemberWithName("next")
        
        if data.IsValid():
            result.AppendMessage(f"  -> [{data.GetValue()}]")
        
        if not next_ptr.IsValid() or next_ptr.GetValueAsUnsigned() == 0:
            result.AppendMessage("  -> NULL")
            break
        
        current = next_ptr.Dereference()
        count += 1
```

## 高级功能

### 事件处理

```python
def handle_stop_event(debugger, command, result, internal_dict):
    def internal_handler(event, breakpoint):
        if event.GetType() == lldb.SBProcess.eStateStopped:
            result.AppendMessage("Process stopped")
    
    listener = debugger.GetListener()
```

### 导出调试信息

```python
import json

def export_debug_info(debugger, command, result, internal_dict):
    target = debugger.GetSelectedTarget()
    process = target.GetProcess()
    
    debug_info = {
        "target": str(target),
        "process_id": process.GetProcessID(),
        "threads": []
    }
    
    for thread in process:
        thread_info = {
            "id": thread.GetThreadID(),
            "frames": []
        }
        
        for frame in thread:
            frame_info = {
                "function": frame.GetFunctionName(),
                "file": frame.GetLineEntry().GetFileSpec().GetFilename(),
                "line": frame.GetLineEntry().GetLine()
            }
            thread_info["frames"].append(frame_info)
        
        debug_info["threads"].append(thread_info)
    
    with open("debug_info.json", "w") as f:
        json.dump(debug_info, f, indent=2)
    
    result.AppendMessage("Debug info exported")
```

## 调试脚本

```python
def debug_script(debugger, command, result, internal_dict):
    try:
        result.AppendMessage("Script executed successfully")
    except Exception as e:
        result.AppendMessage(f"Error: {e}")
        import traceback
        result.AppendMessage(traceback.format_exc())
```

## 最佳实践

1. **错误处理**：始终检查 SBError 和对象有效性
2. **资源管理**：及时释放不需要的资源
3. **性能考虑**：避免在热路径中执行复杂操作
4. **代码组织**：将大型脚本拆分为模块
5. **文档化**：为自定义命令提供清晰的帮助文本

## 总结

LLDB Python 脚本提供了强大的调试自动化能力：

- 核心 API：SBDebugger、SBTarget、SBProcess 等
- 自定义命令：通过 `__lldb_init_module` 注册命令
- 实用脚本：崩溃分析、性能分析、数据可视化
- 高级功能：事件处理、外部集成

通过这些工具，可以显著提高调试效率。