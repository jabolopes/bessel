module Core.Server

use Core.WebServer

type Reply ReplyDirectory @
         | ReplyHtml @

replyDirectory dir@isString = ReplyDirectory dir

replyHtml html@ = ReplyHtml html

