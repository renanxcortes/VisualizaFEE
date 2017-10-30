$(function(){
    $('ul.navbar-nav li a').click(function(){
    //alert($(this).attr('href'));
    var aba = $(this).attr('href');
 
        switch(aba) {
            case "#tab-5606-1": // apresentação
            //alert('apresentação')
            ga('send', 'pageview', '/apresentacao');
            break;
            case "#tab-3013-1": // compara crimes
            //alert('compara crimes')
            ga('send', 'pageview', '/compara-crimes');
            break;
            case "#tab-3013-2": // compara municipios
            // alert('compara municipios')
            ga('send', 'pageview', '/compara-municipios');
            break;
            case "#tab-5606-2": // relação entre crimes
            // alert('relação entre crimes')
            ga('send', 'pageview', '/relacao-entre-crimes');
            break;
            case "#tab-5606-3": // mapa
            //alert('mapa')
            ga('send', 'pageview', '/mapa');
            break;
            case "#tab-5606-4": // mapa
            //alert('representação')
            ga('send', 'pageview', '/representacao');
            break;
            case "#tab-5606-5": // mapa
            //alert('Tabela e Download')
            ga('send', 'pageview', '/tabela-e-download');
            break;
            default:
            // default code block
        }
    });
 
    $('a#downloadData').click(function(){
        ga('send', 'pageview', '/baixar-csv');
    });
 
});