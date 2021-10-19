(function($) {
    $(document).ready(function() {
	
	$('#Iris2').scianimator({
	    'images': ['images/Iris21.png', 'images/Iris22.png', 'images/Iris23.png', 'images/Iris24.png', 'images/Iris25.png', 'images/Iris26.png', 'images/Iris27.png', 'images/Iris28.png', 'images/Iris29.png', 'images/Iris210.png', 'images/Iris211.png', 'images/Iris212.png', 'images/Iris213.png', 'images/Iris214.png', 'images/Iris215.png', 'images/Iris216.png', 'images/Iris217.png', 'images/Iris218.png', 'images/Iris219.png', 'images/Iris220.png'],
	    'width': 480,
	    'delay': 500,
	    'loopMode': 'none',
 'controls': ['first', 'previous', 'play', 'next', 'last', 'loop', 'speed']
	});
	$('#Iris2').scianimator('play');
    });
})(jQuery);
