var client = parseClient();
//trial info
var trialNumber = 0;
var trialData = [];

//canvas parameters
var canvas = document.getElementById("canvas");
var ctx = canvas.getContext("2d");

const width_height = Math.min(innerWidth, innerHeight);
canvas.width = innerHeight * 0.98;
canvas.height = innerHeight * 0.98;
const width_center = canvas.width / 2;
const height_center = canvas.height / 2;

// sub_canvas
// sample the center of two groups from 2-D uniform 
const sub_canvas_center_x_min = canvas.width * 3 / 7;
const sub_canvas_center_x_max = canvas.width * 4 / 7;
const sub_canvas_center_y_min = canvas.height * 3 / 7;
const sub_canvas_center_y_max = canvas.height * 4 / 7;

var sub_canvas_center_list = [];

for (let i = 0; i < 660; i++) {
    sub_canvas_center_list.push(sample_uniform_2d(
        [sub_canvas_center_x_min, sub_canvas_center_x_max],
        [sub_canvas_center_y_min, sub_canvas_center_y_max]));
}


// circle stimuli size
var radius = canvas.width / 120;
var group_range_unit = canvas.width / 24;
var group_mean_to_center = group_range_unit * 6;


var group_1_center_x;
var group_1_center_y;
var group_2_center_x;
var group_2_center_y;

//object features
// var color_set = ["#00ced1", "#ffa500", "#00ff00", "#0000ff", "#ff1493"];
// var color_index = jStat.seq(0, color_set.length - 1, color_set.length);
var color = "#0000ff";

// var shape_set = ["circle", "rectangle", "triangle"];
var shape = "circle";

//response circle
var on = true;


//coordinate initialization
var coord;
var rotated_coord;
var group_1_coord;
var group_2_coord;
var all_coord;

// var mean_group_1_resize;
// var mean_group_2_resize;
// var all_coord_resize;
var response_ratio = {};
var is_resize = 0;

// cardinality given approximately equal axial precision
// 18 conditions 
size_pairs = [
    [1, 16],
    [1, 32],
    [4, 4],
    [4, 8],
    [16, 16],
    [16, 32]
];

// Varying overall precision
range_multiplier = [
    [0.5, 1],
    [1, 1],
    [1, 0.5]
];

// Indexing
var means_index = jStat.seq(0, 11, 12);

// Indexing rule: [group_1_size, group_2_size, range_multiplier, trial index]
// Attention check trial (6)
var attention = [4, 16, 1, 1, -1];

// Practice trial
var practice_1 = [4, 8, 1, 1, -2];
var practice_2 = [4, 8, 0.5, 1, -3];
var practice_3 = [1, 32, 0.5, 1, -4];
var practice_4 = [1, 16, 1, 0.5, -5];
var practice_5 = [16, 32, 0.5, 1, -6];
var practice_6 = [16, 32, 1, 1, -7];

// Trial structure: 6 practice trials + 18*36 = 648 (experimental trials) + 6 attention-check trials = 660 trials
var trial_size_pairs = repeat_whole_single(repeat_each_array(size_pairs, 3), 36);
var trial_size_sd = repeat_whole_single(range_multiplier, 216);
var trial_means_index = repeat_whole_single(repeat_each_single(means_index, 18), 3);
var trial_index = combineArray(trial_size_pairs, trial_size_sd);
console.log(trial_index)
trial_index = addArray(trial_index, trial_means_index);




//attention check
trial_index = shuffle(trial_index);
trial_index.splice(100, 0, attention);
trial_index.splice(200, 0, attention);
trial_index.splice(300, 0, attention);
trial_index.splice(400, 0, attention);
trial_index.splice(500, 0, attention);
trial_index.splice(600, 0, attention);
trial_index.splice(0, 0, practice_6);
trial_index.splice(0, 0, practice_5);
trial_index.splice(0, 0, practice_4);
trial_index.splice(0, 0, practice_3);
trial_index.splice(0, 0, practice_2);
trial_index.splice(0, 0, practice_1);


trial_index = combineArray(trial_index, sub_canvas_center_list);
console.log(trial_index);

//Practice trials

// samples
const samples = JSON.parse(all_samples);

//random index
const size_index_1 = jStat.seq(0, 99, 100);
const size_index_2 = jStat.seq(0, 99, 100);

var rand_index_1;
var rand_index_2;

// group position generator
function get_positions(trial_index) {
    rand_index_1 = randSample(clone(size_index_1), 1);
    rand_index_2 = randSample(clone(size_index_2), 1);
    switch (trial_index[0]) {
        case 1:
            group_1_coord = {
                x: 0,
                y: 0
            };
            break;
        case 2:
            group_1_coord = clone(samples[0].group_sample_1[rand_index_1]);
            break;
        case 4:
            group_1_coord = clone(samples[1].group_sample_1[rand_index_1]);
            break;
        case 8:
            group_1_coord = clone(samples[2].group_sample_1[rand_index_1]);
            break;
        case 16:
            group_1_coord = clone(samples[3].group_sample_1[rand_index_1]);
            break;
        case 32:
            group_1_coord = clone(samples[4].group_sample_1[rand_index_1]);
            break;
    }
    switch (trial_index[1]) {
        case 1:
            group_2_coord = {
                x: 0,
                y: 0
            };
            break;
        case 2:
            group_2_coord = clone(samples[0].group_sample_2[rand_index_2]);
            break;
        case 4:
            group_2_coord = clone(samples[1].group_sample_2[rand_index_2]);
            break;
        case 8:
            group_2_coord = clone(samples[2].group_sample_2[rand_index_2]);
            break;
        case 16:
            group_2_coord = clone(samples[3].group_sample_2[rand_index_2]);
            break;
        case 32:
            group_2_coord = clone(samples[4].group_sample_2[rand_index_2]);
            break;
    }
    //recenter
    if (trial_index[4] == -1) {
        group_1_coord.map(x => x.x = trial_index[5] + x.x * trial_index[2] * (group_range_unit));
        group_1_coord.map(x => x.y = trial_index[6] + x.y * trial_index[2] * (group_range_unit));
        group_2_coord.map(x => x.x = trial_index[5] + x.x * trial_index[2] * (group_range_unit));
        group_2_coord.map(x => x.y = trial_index[6] + x.y * trial_index[2] * (group_range_unit));
        all_coord = group_1_coord.concat(group_2_coord);
        console.log(group_1_coord);
        console.log(group_2_coord);
        console.log(all_coord);
    } else {
        if (trial_index[0] == 1 && trial_index[1] == 1) {
            group_1_coord.x = trial_index[5] + group_mean_to_center * Math.cos(degToRad(180 + trial_index[4] * 30));
            group_1_coord.y = trial_index[6] + group_mean_to_center * Math.sin(degToRad(180 + trial_index[4] * 30));

            group_2_coord.x = trial_index[5] + group_mean_to_center * Math.cos(degToRad(trial_index[4] * 30));
            group_2_coord.y = trial_index[6] + group_mean_to_center * Math.sin(degToRad(trial_index[4] * 30));
            all_coord = [group_1_coord].concat([group_2_coord]);
        } else if (trial_index[0] != 1 && trial_index[1] == 1) {
            group_1_coord.map(x => x.x = (trial_index[5] + group_mean_to_center * Math.cos(degToRad(180 + trial_index[4] * 30))) + x.x * trial_index[2] * (group_range_unit));
            group_1_coord.map(x => x.y = trial_index[6] + group_mean_to_center * Math.sin(degToRad(180 + trial_index[4] * 30)) + x.y * trial_index[2] * (group_range_unit));

            group_2_coord.x = trial_index[5] + group_mean_to_center * Math.cos(degToRad(trial_index[4] * 30));
            group_2_coord.y = trial_index[6] + group_mean_to_center * Math.sin(degToRad(trial_index[4] * 30));

            all_coord = group_1_coord.concat([group_2_coord]);
        } else if (trial_index[0] == 1 && trial_index[1] != 1) {
            group_1_coord.x = trial_index[5] + group_mean_to_center * Math.cos(degToRad(180 + trial_index[4] * 30));
            group_1_coord.y = trial_index[6] + group_mean_to_center * Math.sin(degToRad(180 + trial_index[4] * 30));

            group_2_coord.map(x => x.x = (trial_index[5] + group_mean_to_center * Math.cos(degToRad(trial_index[4] * 30))) + x.x * trial_index[2] * (group_range_unit));
            group_2_coord.map(x => x.y = trial_index[6] + group_mean_to_center * Math.sin(degToRad(trial_index[4] * 30)) + x.y * trial_index[2] * (group_range_unit));

            all_coord = [group_1_coord].concat(group_2_coord);
        } else {
            group_1_coord.map(x => x.x = (trial_index[5] + group_mean_to_center * Math.cos(degToRad(180 + trial_index[4] * 30))) + x.x * trial_index[2] * (group_range_unit));
            group_1_coord.map(x => x.y = trial_index[6] + group_mean_to_center * Math.sin(degToRad(180 + trial_index[4] * 30)) + x.y * trial_index[2] * (group_range_unit));
            group_2_coord.map(x => x.x = (trial_index[5] + group_mean_to_center * Math.cos(degToRad(trial_index[4] * 30))) + x.x * trial_index[3] * (group_range_unit));
            group_2_coord.map(x => x.y = trial_index[6] + group_mean_to_center * Math.sin(degToRad(trial_index[4] * 30)) + x.y * trial_index[3] * (group_range_unit));
            all_coord = group_1_coord.concat(group_2_coord);
        }
    };
    return coord = {
        group_1_coord: group_1_coord,
        group_2_coord: group_2_coord,
        all_coord: all_coord
    };
}
//buttons
var btn = document.getElementById("btn");