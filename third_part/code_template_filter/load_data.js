// import ejs from "ejs";
import Path from "path";
import fs from "fs";
import fse from "fs-extra";
import archiver from "archiver";
import { fileURLToPath } from 'url';
import { dirname } from 'path';
import rbook from "../../src/rbook/index.js";
import { __workdir ,__bookdir} from "../../src/rbook/index.js";
import * as matter from 'gray-matter';

class Markdown {
    constructor(md_path = '') {
        this.name = 'rbook';
        this.front_matter = {};
        this.md_content = '';
        this.html_content = '';
        if( md_path && md_path.length > 0 ) {
            this.md_path = md_path; // md文件路径
            let raw_md = fs.readFileSync(md_path, 'utf8');
            let result = this.matter(raw_md);
            this.front_matter = result.data;
            this.md_content = result.content;
            // this.html_content = this.toHTML(result.content);
        }
    }

    /**
     * 解析FrontMatter和Markdown内容
     * @param {string} md_content - Markdown内容
     * @returns {Object} - { content: markdown, data: frontmatter }
     */
    matter(md_content) {
        try {
            const result = matter.default(md_content);
            return result
        } catch (error) {
            throw new Error(`解析Markdown失败: ${error.message}`);
        }
    }
}


const __dirname = dirname(fileURLToPath(import.meta.url));
//1. 加载数据menu
// import {flatten_menu_json} from "../../src/menu.js";

//2. 加载所有的 template 描述的array
var template_array = []

async function load_data() {
  const RbookEntity = new rbook();
  // console.log(RbookEntity.AllMarkdownFiles)

    //根据信息处理code_template
    /**
     * @param {*} md : Markdown , 这个对象是 `src/rbook/markdown.js` 里面的对象的简化写法
     * @param {*} d : fromt_matter 里面的 code_template 属性, 这个d 就是 code_template 数组里面的一个元素
     *     code_template:
            - title: fhq-treap
              tags: ["平衡树","数据结构"]
              code: /code/data-struture/fhq-treap/fhq.cpp 
     * @param {*} code_template : 完整的code_template 数组
     * @returns
     *  返回一个对象,这个对象包含了code_template的所有信息,但是code_template的code属性被替换成了代码的weburl
     * {
     *  fromt_matter: // 这个是原来的front_matter
     *  md_path: // 这个是原来的md文件的路径
     *  code: // 这个是代码的weburl
     *  title: // 这个是原来的title
     *  tags: // 这个是原来的tags
     *  desc: // 这个是原来的desc
     *  sh: // 这个是原来的sh
     * }
     *  */
    function deal_code_template(md,d,code_template) {
        // console.log("d=>",d)
        // console.log("code_template=>",code_template)
        let file_dir = md.md_path
        // console.log(file_dir)
        let code_path = d.code
        if( Path.isAbsolute(code_path) )
          code_path = Path.join(__workdir,code_path)
        else
          code_path = Path.join(file_dir,d.code)
        
        // 相对__workdir的路径
        let code_path_relative = Path.relative(__workdir,code_path)
        let target_path = Path.join(__dirname,'public',code_path_relative)

        // console.log("code_path: ", code_path)
        // console.log("code_path_relative: ", code_path_relative)
        // console.log("target_path: ", target_path)

        fse.copySync(code_path,target_path)

        //复制代码到public 文件夹下
        let url = code_path_relative
        return {
            ...d,
            //代码的url
            code: url
        }
    }


    // 遍历所有的md文件,得到front_matter
    for( let mdfile of RbookEntity.AllMarkdownFiles) {
        const fullMdPath = Path.join(__bookdir, mdfile);
        console.log(mdfile,fullMdPath)
        if( fs.existsSync(fullMdPath) == false) continue;
        let md = new Markdown(fullMdPath)
        let front_matter = md.front_matter
        if( !front_matter.code_template) continue;
        for(let ct of front_matter.code_template) {
          //  console.log(ct)
            //info 表示原来的数据
            template_array.push({
              ...deal_code_template(md,ct,front_matter.code_template), 
              front_matter,
              md_path: md.md_path
            })
        }
    }
    // console.log(template_array)



    // 不行,因为filter 返回的一个浅拷贝
    // let have_code = flatten_menu_json.filter(d => d.code_template)
    // for (let d of flatten_menu_json) {
    //     if( !d.code_template) continue;
    //     for(let code of d.code_template) {
    //         //info 表示原来的数据
    //         template_array.push({...deal_code_template(d,code), info:d})
    //     }
    // }
    

    // 遍历 template_array ,使用 archiver 创建一个zip文件
    // let zipFiles = []
    // for( let temp of template_array ) {
    //     // console.log(temp)
    //     let title = temp.title
    //     let filename = title + Path.extname(temp.code)
    //     let file_path = Path.join(__dirname,'public',temp.code)
    //     console.log(filename,file_path)
    //     zipFiles.push( {path: file_path,name:filename})
    // }
    
    // const zipfile_path = './public/template.zip'
    // if(fs.existsSync(zipfile_path)) 
    //     fs.rmSync(zipfile_path)
    // await createZipStream(zipfile_path,zipFiles)

    // template_array.unshift({
    //     title: 'all全部代码模板(压缩包)',
    //     tags: [ 'all' ],
    //     desc: '全部的模板',
    //     code: 'template.zip',
    //     sh: 'curl -o template.zip https://rbook.roj.ac.cn/code_template/template.zip',
    //     info: {
    //         md_file: {
    //             href: '/'
    //         },
    //         id: 'alltemplate',
    //         hiden_brain_map: true,
    //         title: '对拍脚本',
    //         file: 'index.md',
    //     }
    // })
}


/**
 * 一次性流式创建ZIP，包含多个文件。
 * @param {string} zipPath - 输出的ZIP文件路径。
 * @param {Array<object>} filesToAdd - 要添加的文件数组，格式：[{path: 'full/path/to/file.txt', name: 'file_in_zip.txt'}]
 */
async function createZipStream(zipPath, filesToAdd) {
  // 创建一个文件写入流
  const output = fs.createWriteStream(zipPath);
  
  // 创建 archiver 实例
  const archive = archiver('zip', {
    zlib: { level: 9 } // 设置压缩级别
  });

  // 监听 'close' 事件，当流关闭时触发
  output.on('close', function() {
    console.log(`[${new Date().toISOString()}] ZIP创建完成: ${zipPath}`);
    console.log(archive.pointer() + ' total bytes');
  });

  // 监听 'error' 事件
  archive.on('error', function(err) {
    throw err;
  });

  // 将 archive 流“管道”连接到文件输出流
  archive.pipe(output);

  // --- 这是“不停添加”的地方 ---
  // 循环添加文件
  for (const fileInfo of filesToAdd) {
    const filePath = fileInfo.path;
    const nameInZip = fileInfo.name;
    
    // 检查文件是否存在
    if (fs.existsSync(filePath)) {
      // 方式一：添加本地文件
      archive.file(filePath, { name: nameInZip });
      
      console.log(`[${new Date().toISOString()}] 正在添加: ${nameInZip}`);
      
      // 模拟添加文件之间的间隔（如果需要的话）
      // await new Promise(resolve => setTimeout(resolve, 100)); // 演示用，实际批量添加时不需要
    } else {
      console.warn(`[${new Date().toISOString()}] 文件未找到，跳过: ${filePath}`);
    }
  }
  
  // --- 完成添加 ---
  // 当所有文件都添加完毕后，必须调用 finalize()
  // 这会写入ZIP的中央目录（索引）并关闭流
  console.log(`[${new Date().toISOString()}] 正在完成ZIP...`);
  await archive.finalize();
}

// --- 使用示例 ---

// const files = [
//   { path: './file1.txt', name: 'file1.txt' },
//   { path: './file2.txt', name: 'inner_folder/file2.txt' },
//   { path: './file3.txt', name: 'file3_renamed.txt' }
// ];
//
// createZipStream('./my-streamed-archive.zip', files);

/*
// 预期输出:
[...] 正在添加: file1.txt
[...] 正在添加: inner_folder/file2.txt
[...] 正在添加: file3_renamed.txt
[...] 正在完成ZIP...
(稍等片刻)
[...] ZIP创建完成: ./my-streamed-archive.zip
[...] total bytes
*/





//2.加载Nodes数据
//需要的数据有
//
// id, label(显示), title()
// group 分组
// href 这个文章的链接
// pre 每个点的前置节点
//
// 下一节点,通常是题目
// next :[
//
// ]
// 还有edge数据
// console.log(flatten_menu_json)
var Nodes = []
var Edges = []
var set = new Set()
var edge_set = new Set()



/*
function load_data(){
    for( let d of flatten_menu_json)
    {
        // 没有id,说明这个节点,没有加入
        if( ! d.id) continue;
        add_node(d)
        if(d.pre) load_pre(d)
        if(d.next) load_next(d)
    }
}
*/

// export const Ns = Nodes
// export const Es = Edges


export default async function nodejsPlugin() {
    await load_data();
    return {
        name: 'nodejs-loadBook-templateCode-plugin',
        config() {
            // 在 Vite 构建过程中执行 Node.js 代码
            // const data = fs.readFileSync('yourNodeJsFile.js', 'utf-8');

            // console.log(template_array)
            return {
                define: {
                    // 将计算得到的数据传递给前端
                    // $yourData: JSON.stringify(data)
                    template_array
                }
            };
        }
    };
}
