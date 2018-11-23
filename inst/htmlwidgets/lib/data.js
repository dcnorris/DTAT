// Hard-code JSON-formatted -data- as placeholder pending r2d3(data=...)

// TODO: Convert these 3 data vars to 1 -data- object {mtd: .., trial: .., ds: ..}
data_mtd = [{"id":1,"mtd":1.8793},{"id":2,"mtd":0.7087},{"id":3,"mtd":1.2785},{"id":4,"mtd":0.6586},{"id":5,"mtd":1.0549},{"id":6,"mtd":1.0445},{"id":7,"mtd":1.1416},{"id":8,"mtd":1.4377},{"id":9,"mtd":0.5804},{"id":10,"mtd":0.3691},{"id":11,"mtd":0.4961},{"id":12,"mtd":1.6563},{"id":13,"mtd":0.4903},{"id":14,"mtd":1.0076},{"id":15,"mtd":0.1939},{"id":16,"mtd":0.843},{"id":17,"mtd":0.3256},{"id":18,"mtd":0.7699},{"id":19,"mtd":1.0569},{"id":20,"mtd":1.2292},{"id":21,"mtd":1.6548},{"id":22,"mtd":0.9855},{"id":23,"mtd":0.4529},{"id":24,"mtd":0.8162}];

data_trial = [{"id":1,"period":1,"dose":1,"dlt":false},{"id":2,"period":1,"dose":1,"dlt":false},{"id":3,"period":1,"dose":1,"dlt":false},{"id":4,"period":2,"dose":1,"dlt":false},{"id":5,"period":2,"dose":1,"dlt":false},{"id":6,"period":2,"dose":1,"dlt":false},{"id":1,"period":2,"dose":2,"dlt":false},{"id":2,"period":2,"dose":2,"dlt":false},{"id":3,"period":2,"dose":2,"dlt":false},{"id":7,"period":3,"dose":1,"dlt":false},{"id":8,"period":3,"dose":1,"dlt":false},{"id":9,"period":3,"dose":1,"dlt":false},{"id":4,"period":3,"dose":2,"dlt":false},{"id":5,"period":3,"dose":2,"dlt":false},{"id":6,"period":3,"dose":2,"dlt":false},{"id":1,"period":3,"dose":3,"dlt":false},{"id":2,"period":3,"dose":3,"dlt":false},{"id":3,"period":3,"dose":3,"dlt":false},{"id":10,"period":4,"dose":1,"dlt":false},{"id":11,"period":4,"dose":1,"dlt":false},{"id":12,"period":4,"dose":1,"dlt":false},{"id":7,"period":4,"dose":2,"dlt":false},{"id":8,"period":4,"dose":2,"dlt":false},{"id":9,"period":4,"dose":2,"dlt":false},{"id":4,"period":4,"dose":3,"dlt":false},{"id":5,"period":4,"dose":3,"dlt":false},{"id":6,"period":4,"dose":3,"dlt":false},{"id":1,"period":4,"dose":4,"dlt":false},{"id":2,"period":4,"dose":4,"dlt":false},{"id":3,"period":4,"dose":4,"dlt":false},{"id":13,"period":5,"dose":2,"dlt":false},{"id":14,"period":5,"dose":2,"dlt":false},{"id":15,"period":5,"dose":2,"dlt":true},{"id":10,"period":5,"dose":2,"dlt":false},{"id":11,"period":5,"dose":2,"dlt":false},{"id":12,"period":5,"dose":2,"dlt":false},{"id":7,"period":5,"dose":3,"dlt":false},{"id":8,"period":5,"dose":3,"dlt":false},{"id":9,"period":5,"dose":3,"dlt":false},{"id":4,"period":5,"dose":4,"dlt":true},{"id":5,"period":5,"dose":4,"dlt":false},{"id":6,"period":5,"dose":4,"dlt":false},{"id":1,"period":5,"dose":5,"dlt":false},{"id":2,"period":5,"dose":5,"dlt":true},{"id":3,"period":5,"dose":5,"dlt":false},{"id":16,"period":6,"dose":2,"dlt":false},{"id":17,"period":6,"dose":2,"dlt":true},{"id":18,"period":6,"dose":2,"dlt":false},{"id":13,"period":6,"dose":3,"dlt":false},{"id":14,"period":6,"dose":3,"dlt":false},{"id":15,"period":6,"dose":1,"dlt":true},{"id":10,"period":6,"dose":3,"dlt":true},{"id":11,"period":6,"dose":3,"dlt":false},{"id":12,"period":6,"dose":3,"dlt":false},{"id":7,"period":6,"dose":4,"dlt":false},{"id":8,"period":6,"dose":4,"dlt":false},{"id":9,"period":6,"dose":4,"dlt":true},{"id":5,"period":6,"dose":5,"dlt":false},{"id":6,"period":6,"dose":5,"dlt":false},{"id":1,"period":6,"dose":5,"dlt":false},{"id":3,"period":6,"dose":5,"dlt":false},{"id":19,"period":7,"dose":2,"dlt":false},{"id":20,"period":7,"dose":2,"dlt":false},{"id":21,"period":7,"dose":2,"dlt":false},{"id":16,"period":7,"dose":3,"dlt":false},{"id":17,"period":7,"dose":1,"dlt":false},{"id":18,"period":7,"dose":3,"dlt":false},{"id":13,"period":7,"dose":4,"dlt":true},{"id":14,"period":7,"dose":4,"dlt":false},{"id":11,"period":7,"dose":4,"dlt":true},{"id":12,"period":7,"dose":4,"dlt":false},{"id":7,"period":7,"dose":5,"dlt":false},{"id":8,"period":7,"dose":5,"dlt":false},{"id":5,"period":7,"dose":6,"dlt":true},{"id":6,"period":7,"dose":6,"dlt":true},{"id":1,"period":7,"dose":6,"dlt":false},{"id":3,"period":7,"dose":6,"dlt":true},{"id":22,"period":8,"dose":2,"dlt":false},{"id":23,"period":8,"dose":2,"dlt":false},{"id":24,"period":8,"dose":2,"dlt":false},{"id":19,"period":8,"dose":3,"dlt":false},{"id":20,"period":8,"dose":3,"dlt":false},{"id":21,"period":8,"dose":3,"dlt":false},{"id":16,"period":8,"dose":4,"dlt":false},{"id":18,"period":8,"dose":4,"dlt":false},{"id":14,"period":8,"dose":5,"dlt":false},{"id":12,"period":8,"dose":5,"dlt":false},{"id":7,"period":8,"dose":6,"dlt":true},{"id":8,"period":8,"dose":6,"dlt":false},{"id":22,"period":9,"dose":3,"dlt":false},{"id":23,"period":9,"dose":3,"dlt":true},{"id":24,"period":9,"dose":3,"dlt":false},{"id":19,"period":9,"dose":4,"dlt":false},{"id":20,"period":9,"dose":4,"dlt":false},{"id":21,"period":9,"dose":4,"dlt":false},{"id":16,"period":9,"dose":5,"dlt":true},{"id":18,"period":9,"dose":5,"dlt":true},{"id":14,"period":9,"dose":6,"dlt":true},{"id":12,"period":9,"dose":6,"dlt":false},{"id":22,"period":10,"dose":4,"dlt":false},{"id":24,"period":10,"dose":4,"dlt":false},{"id":19,"period":10,"dose":5,"dlt":false},{"id":20,"period":10,"dose":5,"dlt":false},{"id":21,"period":10,"dose":5,"dlt":false}];

data_ds = [{"dose":0.5,"surv":0.8571,"upper":0.9714,"lower":0.5148,"period":1},{"dose":1,"surv":0.8571,"upper":0.9714,"lower":0.5148,"period":1},{"dose":0.5,"surv":0.9231,"upper":0.9847,"lower":0.6908,"period":2},{"dose":1,"surv":0.9231,"upper":0.9847,"lower":0.6908,"period":2},{"dose":2,"surv":0.9231,"upper":0.9847,"lower":0.6908,"period":2},{"dose":0.5,"surv":0.9474,"upper":0.9896,"lower":0.7733,"period":3},{"dose":1,"surv":0.9474,"upper":0.9896,"lower":0.7733,"period":3},{"dose":2,"surv":0.9474,"upper":0.9896,"lower":0.7733,"period":3},{"dose":3,"surv":0.9474,"upper":0.9896,"lower":0.7733,"period":3},{"dose":0.5,"surv":0.96,"upper":0.9921,"lower":0.8211,"period":4},{"dose":1,"surv":0.96,"upper":0.9921,"lower":0.8211,"period":4},{"dose":2,"surv":0.96,"upper":0.9921,"lower":0.8211,"period":4},{"dose":3,"surv":0.96,"upper":0.9921,"lower":0.8211,"period":4},{"dose":4,"surv":0.96,"upper":0.9921,"lower":0.8211,"period":4},{"dose":0.5,"surv":0.9032,"upper":0.9639,"lower":0.7653,"period":5},{"dose":2,"surv":0.9032,"upper":0.9639,"lower":0.7653,"period":5},{"dose":3,"surv":0.9032,"upper":0.9639,"lower":0.7653,"period":5},{"dose":3.5,"surv":0.7527,"upper":0.8919,"lower":0.5288,"period":5},{"dose":4,"surv":0.7527,"upper":0.8919,"lower":0.5288,"period":5},{"dose":4.5,"surv":0.5018,"upper":0.7538,"lower":0.2489,"period":5},{"dose":5,"surv":0.5018,"upper":0.7538,"lower":0.2489,"period":5},{"dose":0.5,"surv":0.8889,"upper":0.9529,"lower":0.7599,"period":6},{"dose":2,"surv":0.8889,"upper":0.9529,"lower":0.7599,"period":6},{"dose":2.5,"surv":0.8254,"upper":0.913,"lower":0.6805,"period":6},{"dose":3,"surv":0.8254,"upper":0.913,"lower":0.6805,"period":6},{"dose":3.5,"surv":0.642,"upper":0.7891,"lower":0.4621,"period":6},{"dose":4,"surv":0.642,"upper":0.7891,"lower":0.4621,"period":6},{"dose":4.5,"surv":0.5136,"upper":0.699,"lower":0.3244,"period":6},{"dose":5,"surv":0.5136,"upper":0.699,"lower":0.3244,"period":6},{"dose":0.5,"surv":0.9524,"upper":0.9856,"lower":0.8535,"period":7},{"dose":1.5,"surv":0.9048,"upper":0.9597,"lower":0.7911,"period":7},{"dose":2,"surv":0.9048,"upper":0.9597,"lower":0.7911,"period":7},{"dose":2.5,"surv":0.8482,"upper":0.9248,"lower":0.7175,"period":7},{"dose":3,"surv":0.8482,"upper":0.9248,"lower":0.7175,"period":7},{"dose":3.5,"surv":0.5872,"upper":0.7288,"lower":0.4296,"period":7},{"dose":4,"surv":0.5872,"upper":0.7288,"lower":0.4296,"period":7},{"dose":4.5,"surv":0.5033,"upper":0.6616,"lower":0.3444,"period":7},{"dose":5,"surv":0.5033,"upper":0.6616,"lower":0.3444,"period":7},{"dose":5.5,"surv":0.1258,"upper":0.333,"lower":0.0399,"period":7},{"dose":6,"surv":0.1258,"upper":0.333,"lower":0.0399,"period":7},{"dose":0.5,"surv":0.9583,"upper":0.9875,"lower":0.8705,"period":8},{"dose":1.5,"surv":0.9167,"upper":0.9648,"lower":0.8151,"period":8},{"dose":2,"surv":0.9167,"upper":0.9648,"lower":0.8151,"period":8},{"dose":2.5,"surv":0.8684,"upper":0.9349,"lower":0.7519,"period":8},{"dose":3,"surv":0.8684,"upper":0.9349,"lower":0.7519,"period":8},{"dose":3.5,"surv":0.6368,"upper":0.7637,"lower":0.4876,"period":8},{"dose":4,"surv":0.6368,"upper":0.7637,"lower":0.4876,"period":8},{"dose":4.5,"surv":0.5661,"upper":0.7071,"lower":0.4134,"period":8},{"dose":5,"surv":0.5661,"upper":0.7071,"lower":0.4134,"period":8},{"dose":5.5,"surv":0.1887,"upper":0.3724,"lower":0.0835,"period":8},{"dose":6,"surv":0.1887,"upper":0.3724,"lower":0.0835,"period":8},{"dose":0.5,"surv":0.9583,"upper":0.9875,"lower":0.8705,"period":9},{"dose":1.5,"surv":0.9167,"upper":0.9648,"lower":0.8151,"period":9},{"dose":2.5,"surv":0.8333,"upper":0.9087,"lower":0.7153,"period":9},{"dose":3,"surv":0.8333,"upper":0.9087,"lower":0.7153,"period":9},{"dose":3.5,"surv":0.6481,"upper":0.7632,"lower":0.5129,"period":9},{"dose":4,"surv":0.6481,"upper":0.7632,"lower":0.5129,"period":9},{"dose":4.5,"surv":0.4714,"upper":0.6137,"lower":0.3336,"period":9},{"dose":5.5,"surv":0.1768,"upper":0.3198,"lower":0.0893,"period":9},{"dose":6,"surv":0.1768,"upper":0.3198,"lower":0.0893,"period":9},{"dose":0.5,"surv":0.9583,"upper":0.9875,"lower":0.8705,"period":10},{"dose":1.5,"surv":0.9167,"upper":0.9648,"lower":0.8151,"period":10},{"dose":2.5,"surv":0.8333,"upper":0.9087,"lower":0.7153,"period":10},{"dose":3.5,"surv":0.6667,"upper":0.7758,"lower":0.5362,"period":10},{"dose":4,"surv":0.6667,"upper":0.7758,"lower":0.5362,"period":10},{"dose":4.5,"surv":0.5238,"upper":0.6523,"lower":0.3921,"period":10},{"dose":5,"surv":0.5238,"upper":0.6523,"lower":0.3921,"period":10},{"dose":5.5,"surv":0.1964,"upper":0.3468,"lower":0.1012,"period":10},{"dose":6,"surv":0.1964,"upper":0.3468,"lower":0.1012,"period":10}];

data = { mtd: data_mtd, trial: data_trial, ds: data_ds};
