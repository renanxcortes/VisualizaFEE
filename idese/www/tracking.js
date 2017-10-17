$(function(){
    $('ul.navbar-nav li a').click(function(){
    //alert($(this).attr('href'));
    var aba = $(this).attr('href');
 
        switch(aba) {
            case "#tab-6368-1": // Apresentação
            //alert('Apresentação')
            ga('send', 'pageview', '/apresentacao');
            break;
            case "#tab-2756-1": // Compara Unidades Geográficas
            //alert('Compara Unidades Geográficas')
            ga('send', 'pageview', '/compara-u-geo');
            break;
            case "#tab-2756-2": // Compara Indicadores
            // alert('Compara Indicadores')
            ga('send', 'pageview', '/compara-indicadores');
            break;
            case "#tab-6368-2": // Mapas
            // alert('Mapas')
            ga('send', 'pageview', '/mapa');
            break;
            case "#tab-6368-3": // Relação entre Indicadores
            //alert('Relação entre Indicadores')
            ga('send', 'pageview', '/relacao');
            break;
            case "#tab-6368-4": // Tabela
            //alert('Tabela')
            ga('send', 'pageview', '/tabela');
            break;
            case "#tab-6368-5": // mapa
            //alert('Download')
            ga('send', 'pageview', '/download');
            break;
            default:
            // default code block
        }
    });

     
});