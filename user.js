$(document).ready(function() {
	$(".sidebar").on("activate", function(){
		$(".usermenu li a i").removeClass("icon-white");
		$(".usermenu li.active a i").addClass("icon-white")
	});
	
    $('.usermenu li a').on('click', function(e) {
        e.preventDefault();
        target = this.hash;
        $.scrollTo(target, 500);
   });	
    
    
    //see https://github.com/twitter/bootstrap/issues/6350
    $('[data-clampedwidth]').each(function () {
        var elem = $(this);
        var parentPanel = elem.data('clampedwidth');
        var resizeFn = function () {
            var sideBarNavWidth = $(parentPanel).width() - parseInt(elem.css('paddingLeft')) - parseInt(elem.css('paddingRight')) - parseInt(elem.css('marginLeft')) - parseInt(elem.css('marginRight')) - parseInt(elem.css('borderLeftWidth')) - parseInt(elem.css('borderRightWidth'));
            elem.css('width', sideBarNavWidth);
        };

        resizeFn();
        $(window).resize(resizeFn);
    });    
	
	
});
