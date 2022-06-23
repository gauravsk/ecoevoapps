#' Print footer for Shiny apps
#'
#' This function is only intended for use in EcoEvoApps' shiny apps.
#' @param language which language to print footer in ("en", "es", "ch", "pt", or
#'   "tr")
#' @export
#' @keywords internal
print_app_footer <- function(language = "en"){
  if(language == "pt") {
    cat("#### ***Explore mais aplicativos, forne\\u00e7a feedback ou contribua***\\n\\n*Este \\u00e9 um aplicativo do projeto EcoEvoApps,\\n        uma cole\\u00e7\\u00e3o de aplicativos gratuitos para simular modelos\\n        cl\\u00e1ssicos de teoria ecol\\u00f3gica e evolutiva.\\n        Para acessar mais aplicativos como este,\\n        deixar um coment\\u00e1rio ou opini\\u00e3o, ou contribuir com o projeto,\\n        por favor visite nossa p\\u00e1gina principal \\u2013 https://ecoevoapps.gitlab.io \\u2013\\n        ou nos mande um email para ecoevoapps@gmail.com.*")
#     cat(stri_escape_unicode("#### ***Explore mais aplicativos, forneça feedback ou contribua***
#
# *Este é um aplicativo do projeto EcoEvoApps,
#         uma coleção de aplicativos gratuitos para simular modelos
#         clássicos de teoria ecológica e evolutiva.
#         Para acessar mais aplicativos como este,
#         deixar um comentário ou opinião, ou contribuir com o projeto,
#         por favor visite nossa página principal – https://ecoevoapps.gitlab.io –
#         ou nos mande um email para ecoevoapps@gmail.com.*"))
  } else if (language == "es") {
    cat("#### ***\\u00a1Explore m\\u00e1s aplicaciones, env\\u00ede sus comentarios o contribuya!***\\n\\nEsta es una entrada del proyecto EcoEvoApps,\\n        que es una colecci\\u00f3n de apps gratuitas para simular modelos\\n        te\\u00f3ricos de ecolog\\u00eda y evoluci\\u00f3n. Por favor, explora otras apps\\n        en la colecci\\u00f3n y d\\u00e9janos tus comentarios o contribuye al proyecto:\\n        https://ecoevoapps.gitlab.io o\\n        m\\u00e1ndanos un correo a ecoevoapps@gmail.com.")
#     cat(stri_escape_unicode("#### ***¡Explore más aplicaciones, envíe sus comentarios o contribuya!***
#
# Esta es una entrada del proyecto EcoEvoApps,
#         que es una colección de apps gratuitas para simular modelos
#         teóricos de ecología y evolución. Por favor, explora otras apps
#         en la colección y déjanos tus comentarios o contribuye al proyecto:
#         https://ecoevoapps.gitlab.io o
#         mándanos un correo a ecoevoapps@gmail.com."))
  } else if (language == "ch") {
    cat("#### ***\\u8bf7\\u63a2\\u7d22\\u7cfb\\u5217\\u4e2d\\u7684\\u5176\\u4ed6app\\u3001\\u7ed9\\u6211\\u4eec\\u63d0\\u4f9b\\u53cd\\u9988\\u3001\\u6216\\u5e2e\\u52a9\\u6211\\u4eec\\u6269\\u5145\\u9879\\u76ee\\u5185\\u5bb9***\\n\\u8fd9\\u4e2aapp\\u662fEcoEvoApp\\u9879\\u76ee\\u4e2d\\u7684\\u4e00\\u90e8\\u5206\\u3002\\u8fd9\\u4e2a\\u9879\\u76ee\\u5305\\u542b\\u4e86\\u4e00\\u7cfb\\u5217\\u514d\\u8d39\\u3001\\n        \\u5f00\\u6e90\\u7684app\\uff0c\\u7528\\u4e8e\\u6a21\\u62df\\u7406\\u8bba\\u751f\\u6001\\u548c\\u8fdb\\u5316\\u5b66\\u4e2d\\u7684\\u7ecf\\u5178\\u6a21\\u578b\\u3002\\n        \\u656c\\u8bf7\\u63a2\\u7d22\\u7cfb\\u5217\\u4e2d\\u7684\\u5176\\u4ed6app\\u3001\\u7ed9\\u6211\\u4eec\\u53cd\\u9988\\u3001\\n        \\u5e2e\\u52a9\\u6211\\u4eec\\u6269\\u5145\\u9879\\u76ee\\u5185\\u5bb9\\uff1a https://ecoevoapps.gitlab.io\\u3001\\n        \\u6216\\u901a\\u8fc7\\u90ae\\u4ef6\\u8054\\u7cfb\\u6211\\u4eec\\uff1aecoevoapps@gmail.com.")
#     cat(stri_escape_unicode("#### ***请探索系列中的其他app、给我们提供反馈、或帮助我们扩充项目内容***
# 这个app是EcoEvoApp项目中的一部分。这个项目包含了一系列免费、
#         开源的app，用于模拟理论生态和进化学中的经典模型。
#         敬请探索系列中的其他app、给我们反馈、
#         帮助我们扩充项目内容： https://ecoevoapps.gitlab.io、
#         或通过邮件联系我们：ecoevoapps@gmail.com."))
  } else if (language == "tr") {
    cat("#### ***Aplikasyonlar\\u0131 inceleyin, geri bildirim ya da katk\\u0131 sa\\u011flay\\u0131n!***\\n\\nBu aplikasyon EcoEvoApps projesi dahilindedir.\\n    EcoEvoApps projesi kabul g\\u00f6rm\\u00fc\\u015f teorik ekoloji ve evrimsel\\n    biyoloji modellerini i\\u00e7eren bedava aplikasyonlar koleksiyonudur.\\n    L\\u00fctfen di\\u011fer aplikasyonlar\\u0131m\\u0131z\\u0131 inceleyin ve bize geri bildirimlerinizi\\n    ya da katk\\u0131lar\\u0131n\\u0131z\\u0131 bu adreslerden ula\\u015ft\\u0131r\\u0131n:\\n    https://ecoevoapps.gitlab.io, ecoevoapps@gmail.com.")
#     cat(stri_escape_unicode("#### ***Aplikasyonları inceleyin, geri bildirim ya da katkı sağlayın!***
#
# Bu aplikasyon EcoEvoApps projesi dahilindedir.
#     EcoEvoApps projesi kabul görmüş teorik ekoloji ve evrimsel
#     biyoloji modellerini içeren bedava aplikasyonlar koleksiyonudur.
#     Lütfen diğer aplikasyonlarımızı inceleyin ve bize geri bildirimlerinizi
#     ya da katkılarınızı bu adreslerden ulaştırın:
#     https://ecoevoapps.gitlab.io, ecoevoapps@gmail.com."))

  } else if(language == "en") {
    cat("#### ***Explore more apps, provide feedback, or contribute!***

*This is an entry in the EcoEvoApps project,
      a collection of freely available apps to simulate canonical
      models from theoretical ecology and evolution.
      Please explore other apps in the collection, provide feedback, or
      contribute to the project: https://ecoevoapps.gitlab.io,
      or email us at ecoevoapps@gmail.com.*")
  } else {
    stop("Please provide a supported language (ch, es, en, tr, or pt)")
  }
}
