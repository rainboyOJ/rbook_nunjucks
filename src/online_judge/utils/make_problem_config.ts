//创建题目的 config.json 数据

class ProblemConfig {
    title?: string;
    memory: number;
    time: number;
    tags: string[];
    source?: string;
    oj: string;
    id?: string;
    [key: string]: unknown;

    constructor() {
        this.title = undefined
        this.memory = 128
        this.time = 128
        this.tags = []
        this.source = undefined
        this.oj = 'roj'
        this.id = undefined
    }

    toObject() {
        return {...this}
    }

    set(name: string, val: unknown) {
        this[name] = val
        return this
    }

    set_source(val: string) {
        return this.set('source', val)
    }

    add_tags(tag: string) {
        this.tags.push(tag)
        return this
    }

    toJSON(){
        return JSON.stringify(this.toObject())
    }

}

export default ProblemConfig
export { ProblemConfig }
