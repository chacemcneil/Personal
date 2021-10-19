(function($) {
    $(document).ready(function() {
	
	$('#Iris').scianimator({
	    'images': ['images/Iris1.png', 'images/Iris2.png', 'images/Iris3.png', 'images/Iris4.png', 'images/Iris5.png', 'images/Iris6.png', 'images/Iris7.png', 'images/Iris8.png', 'images/Iris9.png', 'images/Iris10.png', 'images/Iris11.png', 'images/Iris12.png', 'images/Iris13.png', 'images/Iris14.png', 'images/Iris15.png', 'images/Iris16.png', 'images/Iris17.png', 'images/Iris18.png', 'images/Iris19.png', 'images/Iris20.png'],
	    'width': 480,
	    'delay': 500,
	    'loopMode': 'none',
 'controls': ['first', 'previous', 'play', 'next', 'last', 'loop', 'speed']
	});
	$('#Iris').scianimator('play');
    });
})(jQuery);
