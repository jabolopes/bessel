me Core.Web

use Core.Html

type Attribute Attr [@, @]

let attribute attr@ val@ =
  Attr [attr, val]

let class      attribute "class"
let content    attribute "content"
let href       attribute "href"
let http-equiv attribute "httpEquiv"
let name       attribute "name"
let rel        attribute "rel"
let src        attribute "src"
let style      attribute "style"
let type#      attribute "type#"
let width      attribute "width"

let mkSingleTag id

let mkTag
  tag@ (Attribute [attr@, val@]) =
    mkTag (Html.applyAttribute tag attr val)
| tag@ val@isString =
    mkSingleTag (Html.applyTag tag (Html.string val))
| tag@ vals@((const true) +> (const true)) =
    mkSingleTag (Html.applyList tag vals)
| tag@ val@ =
    mkSingleTag (Html.applyTag tag val)

let (!)
  tag@ (Attribute [attr@, val@]) =
    mkSingleTag (Html.applyAttribute tag attr val)

let a           mkTag Html.a
let abbr        mkTag Html.abbr
let address     mkTag Html.address
let area        mkSingleTag Html.area
let article     mkTag Html.article
let aside       mkTag Html.aside
let audio       mkTag Html.audio
let b           mkTag Html.b
let base        mkSingleTag Html.base
let bdo         mkTag Html.bdo
let blockquote  mkTag Html.blockquote
let body        mkTag Html.body
let br          mkSingleTag Html.br
let button      mkTag Html.button
let canvas      mkTag Html.canvas
let caption     mkTag Html.caption
let cite        mkTag Html.cite
let code        mkTag Html.code
let col         mkSingleTag Html.col
let colgroup    mkTag Html.colgroup
let command     mkTag Html.command
let datalist    mkTag Html.datalist
let dd          mkTag Html.dd
let del         mkTag Html.del
let details     mkTag Html.details
let dfn         mkTag Html.dfn
let dl          mkTag Html.dl
let docType     mkSingleTag Html.docType
let docTypeHtml mkTag Html.docTypeHtml
let dt          mkTag Html.dt
let em          mkTag Html.em
let embed       mkSingleTag Html.embed
let fieldset    mkTag Html.fieldset
let figcaption  mkTag Html.figcaption
let figure      mkTag Html.figure
let footer      mkTag Html.footer
let form        mkTag Html.form
let h1          mkTag Html.h1
let h2          mkTag Html.h2
let h3          mkTag Html.h3
let h4          mkTag Html.h4
let h5          mkTag Html.h5
let h6          mkTag Html.h6
let head        mkTag Html.head
let header      mkTag Html.header
let hgroup      mkTag Html.hgroup
let hr          mkSingleTag Html.hr
let html        mkTag Html.html
let i           mkTag Html.i
let iframe      mkTag Html.iframe
let img         mkSingleTag Html.img
let input       mkSingleTag Html.input
let ins         mkTag Html.ins
let kbd         mkTag Html.kbd
let keygen      mkSingleTag Html.keygen
let label       mkTag Html.label
let legend      mkTag Html.legend
let li          mkTag Html.li
let link        mkSingleTag Html.link
let map         mkTag Html.map
let mark        mkTag Html.mark
let menu        mkTag Html.menu
let meta        mkSingleTag Html.meta
let meter       mkTag Html.meter
let nav         mkTag Html.nav
let noscript    mkTag Html.noscript
let object      mkTag Html.object
let ol          mkTag Html.ol
let optgroup    mkTag Html.optgroup
let option      mkTag Html.option
let output      mkTag Html.output
let p           mkTag Html.p
let param       mkSingleTag Html.param
let pre         mkTag Html.pre
let progress    mkTag Html.progress
let q           mkTag Html.q
let rp          mkTag Html.rp
let rt          mkTag Html.rt
let ruby        mkTag Html.ruby
let samp        mkTag Html.samp
let script      mkTag Html.script
let section     mkTag Html.section
let select      mkTag Html.select
let small       mkTag Html.small
let source      mkSingleTag Html.source
let span        mkTag Html.span
let strong      mkTag Html.strong
-- let style       tag Html.style
-- let sub         tag Html.sub
let summary     mkTag Html.summary
let sup         mkTag Html.sup
let table       mkTag Html.table
let tbody       mkTag Html.tbody
let td          mkTag Html.td
let textarea    mkTag Html.textarea
let tfoot       mkTag Html.tfoot
let th          mkTag Html.th
let thead       mkTag Html.thead
let time        mkTag Html.time
let title       mkTag Html.title
let tr          mkTag Html.tr
let ul          mkTag Html.ul
let var         mkTag Html.var
let video       mkTag Html.video

let renderHtml Html.renderHtml
