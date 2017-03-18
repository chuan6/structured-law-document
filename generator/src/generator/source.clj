(ns generator.source
  (require [clojure.string :as str]))

(defn txt-page-pair [n]
  [(str n ".txt")
   (str n ".html")])

(def name-link-pairs
  [
   ["民法总则"
    "http://www.npc.gov.cn/npc/xinwen/2017-03/15/content_2018907.htm"]

   ["互联网直播服务管理规定"
    "http://www.cac.gov.cn/2016-11/04/c_1119846202.htm"]

   ["治安管理处罚法"
    "http://www.npc.gov.cn/wxzl/gongbao/2013-02/25/content_1790854.htm"]

   ["上海市网络预约出租汽车经营服务管理若干规定（草案）"
    "http://jtj.sh.gov.cn/jtw/gsgg/n39/u1ai51894.html"]

   ["刑事诉讼法"
    "http://www.gov.cn/flfg/2012-03/17/content_2094354.htm"]

   ["侵权责任法"
    "http://www.npc.gov.cn/wxzl/gongbao/2010-03/01/content_1580399.htm"]

   ["民法总则（草案）（二次审议稿）"
    "http://www.npc.gov.cn/COBRS_LFYJNEW/user/UserIndex.jsp?ID=8145189"]

   ["著作权法（2010年修正）"
    "http://www.sipo.gov.cn/zcfg/flfg/bq/fl/201509/t20150911_1174554.html"]

   ["反家庭暴力法"
    "http://www.npc.gov.cn/npc/xinwen/2015-12/28/content_1957457.htm"]

   ["网络安全法"
    "http://www.npc.gov.cn/npc/xinwen/2016-11/07/content_2001605.htm"]

   ["香港特别行政区基本法"
    "http://www.npc.gov.cn/wxzl/gongbao/1990-04/04/content_1479210.htm"]

   ["党内监督条例"
    "http://www.spp.gov.cn/zdgz/201611/t20161103_171442.shtml"]

   ["劳动合同法"
    "http://www.npc.gov.cn/wxzl/gongbao/2013-04/15/content_1811058.htm"]

   ["网络预约出租汽车经营服务管理暂行办法"
    "http://zizhan.mot.gov.cn/zfxxgk/bnssj/zcfgs/201607/t20160728_2068633.html"]

   ["高等教育法"
    "http://www.moe.edu.cn/publicfiles/business/htmlfiles/moe/moe_619/200407/1311.html"]

   ["种子法"
    "http://www.forestry.gov.cn/Zhuanti/content_lqgg/817306.html"]

   ["体育法"
    "http://www.sport.gov.cn/n16/n1092/n16819/312031.html"]

   ["婚姻法"
    "http://www.npc.gov.cn/wxzl/gongbao/2001-05/30/content_5136774.htm"]

   ["合同法"
    "http://www.npc.gov.cn/wxzl/wxzl/2000-12/06/content_4732.htm"]

   ["民法通则"
    "http://www.npc.gov.cn/wxzl/wxzl/2000-12/06/content_4470.htm"]

   ["网络借贷信息中介机构业务活动管理暂行办法"
    "http://www.cbrc.gov.cn/chinese/home/docDOC_ReadView/D934AAE7E05849D185CD497936D767CF.html"]

   ["互联网广告管理暂行办法"
    "http://www.saic.gov.cn/zwgk/zyfb/zjl/xxzx/201607/t20160708_169638.html"]

   ["个体工商户条例"
    "http://www.gov.cn/zwgk/2011-04/28/content_1853972.htm"]

   ["出口退（免）税企业分类管理办法"
    "http://www.chinatax.gov.cn/n810341/n810755/c2217201/content.html"]

   ["食品安全法"
    "http://www.gov.cn/zhengce/2015-04/25/content_2853643.htm"]

   ["立法法"
    "http://www.npc.gov.cn/npc/dbdhhy/12_3/2015-03/18/content_1930713.htm"]

   ["宪法"
    "http://www.npc.gov.cn/npc/xinwen/node_505.htm"]

   ["劳动法"
    "http://www.npc.gov.cn/wxzl/gongbao/2000-12/05/content_5004622.htm"]

   ["公司法"
    "http://www.saic.gov.cn/zcfg/fl/xxb/201402/t20140227_142232.html"]

   ["婚姻登记条例"
    "http://www.gov.cn/zwgk/2005-05/23/content_167.htm"]

   ["道路交通安全法"
    "http://www.npc.gov.cn/npc/xinwen/2011-04/23/content_1653570.htm"]

   ["工伤保险条例"
    "http://www.gov.cn/zwgk/2005-05/20/content_144.htm"]

   ["继承法"
    "http://www.npc.gov.cn/wxzl/wxzl/2000-12/06/content_4457.htm"]

   ["国有土地上房屋征收与补偿条例"
    "http://www.gov.cn/flfg/2011-01/21/content_1791480.htm"]

   ["消费者权益保护法"
    "http://www.npc.gov.cn/wxzl/gongbao/2014-01/02/content_1823351.htm"]

   ["农村土地承包法"
    "http://www.npc.gov.cn/wxzl/gongbao/2002-08/30/content_5299419.htm"]

   ["房产税暂行条例"
    "http://www.gov.cn/banshi/2005-08/19/content_24823.htm"]

   ["合伙企业法"
    "http://www.npc.gov.cn/wxzl/wxzl/2006-09/26/content_354975.htm"]

   ["收养法"
    "http://www.npc.gov.cn/wxzl/gongbao/2000-12/17/content_5004717.htm"]

   ["物业管理条例"
    "http://www.npc.gov.cn/npc/xinwen/fztd/fggz/2007-09/03/content_371389.htm"]

   ["教育法"
    "http://www.moe.edu.cn/s78/A02/zfs__left/s5911/moe_619/201512/t20151228_226193.html"]

   ["物权法"
    "http://www.npc.gov.cn/wxzl/gongbao/2007-03/16/content_5366956.htm"]

   ["刑法（2015修正）"
    "http://www.lawtime.cn/faguizt/23.html"]])
