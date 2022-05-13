#' Print footer for Shiny apps
#'
#' This function is only intended for use in EcoEvoApps' shiny apps.
#' @param language which language to print footer in ("en", "es", "ch", "pt", or
#'   "tr")
#' @export
#' @keywords internal
print_app_footer <- function(language = "en"){
  if(language == "pt") {
    cat("#### ***Explore mais aplicativos, forneça feedback ou contribua***

*Este é um aplicativo do projeto EcoEvoApps,
        uma coleção de aplicativos gratuitos para simular modelos
        clássicos de teoria ecológica e evolutiva.
        Para acessar mais aplicativos como este,
        deixar um comentário ou opinião, ou contribuir com o projeto,
        por favor visite nossa página principal – https://ecoevoapps.gitlab.io –
        ou nos mande um email para ecoevoapps@gmail.com.*")
  } else if (language == "es") {
    cat("#### ***¡Explore más aplicaciones, envíe sus comentarios o contribuya!***

Esta es una entrada del proyecto EcoEvoApps,
        que es una colección de apps gratuitas para simular modelos
        teóricos de ecología y evolución. Por favor, explora otras apps
        en la colección y déjanos tus comentarios o contribuye al proyecto:
        https://ecoevoapps.gitlab.io o
        mándanos un correo a ecoevoapps@gmail.com.")
  } else if (language == "ch") {
    cat("#### ***e***
这个app是EcoEvoApp项目中的一部分。这个项目包含了一系列免费、
        开源的app，用于模拟理论生态和进化学中的经典模型。
        敬请探索系列中的其他app、给我们反馈、
        帮助我们扩充项目内容： https://ecoevoapps.gitlab.io、
        或通过邮件联系我们：ecoevoapps@gmail.com。")
  } else if (language == "tr") {
    "#### ***Aplikasyonları inceleyin, geri bildirim ya da katkı sağlayın!***

Bu aplikasyon EcoEvoApps projesi dahilindedir.
    EcoEvoApps projesi kabul görmüş teorik ekoloji ve evrimsel
    biyoloji modellerini içeren bedava aplikasyonlar koleksiyonudur.
    Lütfen diğer aplikasyonlarımızı inceleyin ve bize geri bildirimlerinizi
    ya da katkılarınızı bu adreslerden ulaştırın:
    https://ecoevoapps.gitlab.io, ecoevoapps@gmail.com."
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
