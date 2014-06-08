function assign_condition(callback) {

    var targets = [
        "TRAIN-SD-HD", 
        "TRAIN-SD-LD", 
        "TRAIN-XAB-HD", 
        "TRAIN-SIM-HD", 
        "TRAIN-XAB-LD", 
        "TRAIN-SIM-LD", 
        "CONTROL-SD-HD", 
        "CONTROL-SD-LD", 
        "CONTROL-XAB-HD", 
        "CONTROL-SIM-HD", 
        "CONTROL-XAB-LD", 
        "CONTROL-SIM-LD"
    ];

    //var cond = targets[Math.floor(Math.random() * targets.length)];
    
    $.ajax({
		type: 'post',
		cache: false,
		url: 'assign_condition.php',
		data: { },
		success: function(data) { 
		    
		    var cond = targets[parseInt(data)];
		    
		    // fill out stim object
            var cond_arr = cond.split("-");
            var condition = {
                "stim": cond_arr[2],
                "test": cond_arr[1],
                "train": cond_arr[0]
            };
            
            callback(condition);
		}
	});
    
}