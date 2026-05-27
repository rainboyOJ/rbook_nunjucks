//创建题目的 config.json 数据

class ProblemConfig {

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

    set(name,val) {
        this[name] = val
        return this
    }

    set_source(val) {
        return this.set('source', val)
    }

    add_tags(tag) {
        this.tags.push(tag)
        return this
    }

    toJSON(){
        return JSON.stringify(this.toObject())
    }

}

export default ProblemConfig
export { ProblemConfig }
