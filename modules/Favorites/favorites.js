/*AJAX turn off the async for ajax*/
$.ajaxSetup({"async": false});

/*Grab elements needed for functions*/
var ajaxUri = "index.php?module=Favorites";
var favorite_icon_outline = $('.favorite_icon_outline');
var favorite_icon_fill = $('.favorite_icon_fill');
var favorite_id = $('.favorite').attr("record_id");
var favorite_module = $('.favorite').attr("module");

/*Initially check if the current record is a favorite.*/
check_favorite();

/*handles a user clicking on the empty star to add a favorite*/
$(favorite_icon_outline).unbind('click.favorite').bind('click.favorite', markFavorite);

/*handles a user clicking on the filled star to remove a favorite*/
$(favorite_icon_fill).unbind('click.favorite').bind('click.favorite', unmarkFavorite);

function unmarkFavorite() {
    remove_record();
    show_star(favorite_icon_outline,favorite_icon_fill);
    remove_favorite_sidebar();
}

function markFavorite() {
    create_record();
    show_star(favorite_icon_fill,favorite_icon_outline);
    get_sidebar_elements();
}

/* Takes the html for a sidebar element and appends it to the sidebar.*/
function add_favorite_sidebar(sidebar_element){
    $('#favoritesSidebar ul').prepend(sidebar_element);
}

/*Finds the related record in the sidebar and removes the html*/
function remove_favorite_sidebar() {
    $('#' + favorite_id + '_favorite').remove();
}

/*Shows or hides the relevant star icon passed in*/
function show_star(show,hide){
    $(show).show();
    $(hide).hide();
}

/*AJAX call to create a favorite record*/
function create_record(){
    var param = {
        action: "create_record",
        record_id: favorite_id,
        record_module: favorite_module,
        to_pdf: true
    };
    handle_response($.getJSON(ajaxUri, param));
}

/*AJAX call to if the current record is an active favorite for the current user*/
function check_favorite() {

    var param = {
        action: "check_favorite",
        record_id: favorite_id,
        record_module: favorite_module,
        to_pdf: true
    };
    handle_response($.getJSON(ajaxUri, param));

}

/*AJAX call to remove the current favorite record*/
function remove_record(){
    var param = {
        action: "remove_record",
        record_id: favorite_id,
        record_module: favorite_module,
        to_pdf: true
    };
    handle_response($.getJSON(ajaxUri, param));
}


/*AJAX basic response handling for create, remove and check favorite ajax requests to show relevant stars*/
function handle_response(data){

    if(data.responseText != "false"){
        show_star(favorite_icon_fill,favorite_icon_outline);
    }else{
        show_star(favorite_icon_outline,favorite_icon_fill);
    }
}

/*AJAX call to get all the elements required to add element to sidebar*/
function get_sidebar_elements(){
    var param = {
        action: "get_sidebar_elements",
        record_id: favorite_id,
        record_module: favorite_module,
        to_pdf: true
    };
    add_favorite_sidebar(format_sidebar_elements($.getJSON(ajaxUri, param)));
}

/*AJAX format the sidebar elements with html to be added to the sidebar*/
function format_sidebar_elements(data){
    var elements = JSON.parse(data.responseText);
    elements = elements[0];

    var div_start = "<div class='recently_viewed_link_container_sidebar new' id='" + elements.id +"_favorite'>";
    var edit_link = "<li class='recentlinks_edit'><a href='index.php?module=" + elements.module_name + "&action=EditView&record=" + elements.id + "'><span class=' glyphicon glyphicon-pencil' aria-hidden='true'></span></a></li>";
    var detail_link = "<li class='recentlinks' role='presenation'><a href='index.php?module=" + elements.module_name + "&action=DetailView&record=" + elements.id + "'>" + elements.image + "<span aria-hidden='true'> " + elements.item_summary_short + "</span></a></li>";
    var div_close = "</div>";

    return div_start + edit_link + detail_link + div_close;
}
