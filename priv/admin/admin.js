/*
 * Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
 * All rights reserved.
 * Distributed under the terms of the MIT License. See the LICENSE file.
 */
var myApp = angular.module('myApp', ['ng-admin', 'uiGmapgoogle-maps', 'googlechart']);
myApp.config(['NgAdminConfigurationProvider', function (nga) {
    var admin = nga.application('Server Admin').baseApiUrl('/');

    var applications = nga.entity('applications')
        .identifier(nga.field('name'));
    var users = nga.entity('users')
        .identifier(nga.field('name'));
    var gateways = nga.entity('gateways')
        .identifier(nga.field('mac'));
    var devices = nga.entity('devices')
        .identifier(nga.field('deveui'));
    var links = nga.entity('links')
        .identifier(nga.field('devaddr'));
    var txframes = nga.entity('txframes')
        .identifier(nga.field('frid'));
    var rxframes = nga.entity('rxframes')
        .identifier(nga.field('devaddr'))
        .readOnly();

    on_off_choices = [
        { value: 0, label: 'OFF' },
        { value: 1, label: 'ON' },
    ];

    // ---- EU863-870 interpretation

    data_rate_choices = [
        { value: 0, label: 'SF12 125 kHz (250 bit/s)' },
        { value: 1, label: 'SF11 125 kHz (440 bit/s)' },
        { value: 2, label: 'SF10 125 kHz (980 bit/s)' },
        { value: 3, label: 'SF9 125 kHz (1760 bit/s)' },
        { value: 4, label: 'SF8 125 kHz (3125 bit/s)' },
        { value: 5, label: 'SF7 125 kHz (5470 bit/s)' },
        { value: 6, label: 'SF7 250 kHz (11000 bit/s)' }
    ];

    power_choices = [
        { value: 0, label: '20 dBm' },
        { value: 1, label: '14 dBm' },
        { value: 2, label: '11 dBm' },
        { value: 3, label: '8 dBm' },
        { value: 4, label: '5 dBm' },
        { value: 5, label: '2 dBm' }
    ];

    // ---- users
    users.listView().fields([
        nga.field('name').isDetailLink(true)
    ]);
    users.creationView().fields([
        nga.field('name'),
        nga.field('pass', 'password')
    ]);
    users.editionView().fields(users.creationView().fields());
    // add to the admin application
    admin.addEntity(users);

    // ---- gateways
    gateways.listView().fields([
        nga.field('mac').label('MAC').isDetailLink(true),
        nga.field('netid').label('NetID')
    ]);
    gateways.creationView().fields([
        nga.field('mac').label('MAC')
            .attributes({ placeholder: 'e.g. 0123456789ABCDEF' })
            .validation({ required: true, pattern: '[A-Za-z0-9]{16}' }),
        nga.field('netid').label('NetID')
            .attributes({ placeholder: 'e.g. 0123AB' })
            .validation({ required: true, pattern: '[A-Za-z0-9]{6}' }),
        nga.field('gpspos', 'template')
            .validation({required: true })
            .label('Location')
            .template('<map location="value"></map>'),
        nga.field('gpsalt', 'number').label('Altitude')
    ]);
    gateways.editionView().fields(gateways.creationView().fields());
    // add to the admin application
    admin.addEntity(gateways);

    // ---- devices
    devices.listView().fields([
        nga.field('deveui').label('DevEUI').isDetailLink(true),
        nga.field('app').label('Application'),
        nga.field('appid').label('AppID'),
        nga.field('appeui').label('AppEUI'),
        nga.field('appkey').label('AppKey'),
        nga.field('link', 'reference')
            .targetEntity(links)
            .targetField(nga.field('devaddr'))
    ]);
    devices.creationView().fields([
        nga.field('deveui').label('DevEUI')
            .attributes({ placeholder: 'e.g. 0123456789ABCDEF' })
            .validation({ required: true, pattern: '[A-Za-z0-9]{16}' }),
        nga.field('app', 'reference').label('Application')
            .targetEntity(applications)
            .targetField(nga.field('name'))
            .validation({ required: true }),
        nga.field('appid').label('AppID'),
        nga.field('appeui').label('AppEUI')
            .attributes({ placeholder: 'e.g. 0123456789ABCDEF' })
            .validation({ required: true, pattern: '[A-Za-z0-9]{16}' }),
        nga.field('appkey').label('AppKey')
            .attributes({ placeholder: 'e.g. FEDCBA9876543210FEDCBA9876543210' })
            .validation({ required: true, pattern: '[A-Za-z0-9]{32}' }),
        nga.field('link')
            .attributes({ placeholder: 'e.g. ABC12345' })
            .validation({ pattern: '[A-Za-z0-9]{8}' }),
        nga.field('adr_flag_set', 'choice').label('Set ADR')
            .choices(on_off_choices)
            .defaultValue(1), // ON
        nga.field('adr_set.power', 'choice').label('Set power')
            .choices(power_choices)
            .defaultValue(1), // 14 dBm
        nga.field('adr_set.datr', 'choice').label('Set data rate')
            .choices(data_rate_choices) // DR0
            .defaultValue(0),
        nga.field('adr_set.chans', 'template').label('Set channels')
            .template('<channels field="field" count="16" value="value"></channels>')
    ]);
    devices.editionView().fields(devices.creationView().fields());
    // add to the admin application
    admin.addEntity(devices);

    // ---- links
    links.listView().fields([
        nga.field('devaddr').label('DevAddr').isDetailLink(true),
        nga.field('app').label('Application'),
        nga.field('appid').label('AppID'),
        nga.field('nwkskey').label('NwkSKey'),
        nga.field('appskey').label('AppSKey'),
        nga.field('fcntup', 'number').label('FCnt Up'),
        nga.field('fcntdown', 'number').label('FCnt Down')
    ]);
    links.creationView().fields([
        nga.field('devaddr').label('DevAddr')
            .attributes({ placeholder: 'e.g. ABC12345' })
            .validation({ required: true, pattern: '[A-Za-z0-9]{8}' }),
        nga.field('app', 'reference').label('Application')
            .targetEntity(applications)
            .targetField(nga.field('name'))
            .validation({ required: true }),
        nga.field('appid').label('AppID'),
        nga.field('nwkskey').label('NwkSKey')
            .attributes({ placeholder: 'e.g. FEDCBA9876543210FEDCBA9876543210' })
            .validation({ required: true, pattern: '[A-Za-z0-9]{32}' }),
        nga.field('appskey').label('AppSKey')
            .attributes({ placeholder: 'e.g. FEDCBA9876543210FEDCBA9876543210' })
            .validation({ required: true, pattern: '[A-Za-z0-9]{32}' }),
        nga.field('fcntup', 'number').label('FCnt Up')
            .defaultValue(0),
        nga.field('fcntdown', 'number').label('FCnt Down')
            .defaultValue(0),
        nga.field('adr_flag_set', 'choice').label('Set ADR')
            .choices(on_off_choices)
            .defaultValue(1), // ON
        nga.field('adr_set.power', 'choice').label('Set power')
            .choices(power_choices)
            .defaultValue(1), // 14 dBm
        nga.field('adr_set.datr', 'choice').label('Set data rate')
            .choices(data_rate_choices) // DR0
            .defaultValue(0),
        nga.field('adr_set.chans', 'template').label('Set channels')
            .template('<channels field="field" count="16" value="value"></channels>')
    ]);
    links.editionView().fields(
        links.creationView().fields().concat([
        nga.field('adr_flag_use', 'choice').label('Used ADR')
            .choices(on_off_choices)
            .editable(false),
        nga.field('adr_use.power', 'choice').label('Used power')
            .choices(power_choices)
            .editable(false),
        nga.field('adr_use.datr', 'choice').label('Used data rate')
            .choices(data_rate_choices)
            .editable(false),
        nga.field('adr_use.chans', 'template').label('Used channels')
            .template('<channels field="field" count="16" value="value"></channels>')
            .editable(false),
        nga.field('devaddr', 'template').label('RX')
            .template('<rgraph value="value"></rgraph>'),
        nga.field('devaddr', 'template').label('RX Quality')
            .template('<qgraph value="value"></qgraph>'),
        nga.field('downlinks', 'referenced_list')
            .targetEntity(txframes)
            .targetReferenceField('devaddr')
            .targetFields([
                nga.field('datetime').label('Creation Time'),
                nga.field('port'),
                nga.field('data')
            ])
            .listActions(['delete'])
    ]));
    // add to the admin application
    admin.addEntity(links);
    admin.addEntity(txframes);

    // ---- menu
    admin.menu(nga.menu()
        .addChild(nga.menu(users).icon('<span class="fa fa-user fa-fw"></span>'))
        .addChild(nga.menu(gateways).icon('<span class="fa fa-cloud fa-fw"></span>'))
        .addChild(nga.menu(devices).icon('<span class="fa fa-cube fa-fw"></span>'))
        .addChild(nga.menu(links).icon('<span class="fa fa-rss fa-fw"></span>'))
    );

    // ---- dashboard
    admin.dashboard(nga.dashboard()
        .addCollection(nga.collection(devices)
            .fields([
                nga.field('deveui').label('DevEUI').isDetailLink(true)
            ])
        )
        .addCollection(nga.collection(links)
            .fields([
                nga.field('devaddr').label('DevAddr').isDetailLink(true)
            ])
        )
    );

    // attach the admin application to the DOM and execute it
    nga.configure(admin);
}]);

myApp.directive('channels', [function () {
return {
    restrict: 'E',
    scope: {
        field: '&',
        count: '=',
        value: '=',
    },
    link: function($scope) {
        const field = $scope.field();
        if ($scope.value == undefined) {
            $scope.value = '111';
        }
        $scope.name = field.name();
        $scope.readonly = !field.editable();
        $scope.bits = [];
        for (var i = 0; i < $scope.count; i++) {
            $scope.bits.push({id:i, val:(i < $scope.value.length ? $scope.value[$scope.value.length-1-i] == '1' : false)});
        }
        $scope.bits = $scope.bits.reverse();
        $scope.change = function() {
            $scope.value = '';
            for (var i = 0; i < $scope.count; i++) {
                $scope.value += $scope.bits[i].val ? '1' : '0';
            }
        }
    },
    template:
    `
    <p ng-repeat="bit in bits" class="channel_check">
      <input type="checkbox" id="{{name}}bit{{bit.id}}" ng-model="bit.val" ng-disabled="readonly" ng-change="change()"/>
      <label for="{{name}}bit{{bit.id}}">{{bit.id}}</label>
    </p>
    `
};}]);

// http://stackoverflow.com/questions/35895411/ng-admin-and-google-maps
myApp.directive('map', [function () {
return {
    restrict: 'E',
    scope: {
        value: '=location',
    },
    link: function($scope, uiGmapIsReady) {
        if ($scope.value == undefined) {
            $scope.value = { lat: 48.88, lon: 14.12};
        }
        $scope.map = { center: { latitude: $scope.value.lat, longitude: $scope.value.lon }, zoom: 4 };
        $scope.marker = {
            id: 0,
            coords: {
                latitude: $scope.value.lat,
                longitude: $scope.value.lon
            },
            options: { draggable: true },
            events: {
                dragend: function (marker, eventName, args) {
                    $scope.value = { lat: marker.getPosition().lat(), lon: marker.getPosition().lng() };
                }
            }
        };
    },
    template:
    `
    <div class="row list-view">
        <div class="col-lg-12">
            <ui-gmap-google-map center="map.center" zoom="map.zoom" draggable="true" options="options" pan=true refresh="true">
                <ui-gmap-marker coords="marker.coords" options="marker.options" events="marker.events" idkey="marker.id"/>
            </ui-gmap-google-map>
        </div>
    </div>
    `
};}]);

myApp.config(function (uiGmapGoogleMapApiProvider) {
    uiGmapGoogleMapApiProvider.configure({
        key: '',
        v: '3',
        libraries: 'visualization'
    });
});

myApp.directive('rgraph', ['$http', '$interval', function($http, $interval) {
return {
    restrict: 'E',
    scope: {
        value: '=',
    },
    link: function($scope) {
            function updateData() {
                $http({method: 'GET', url: '/rx/'.concat($scope.value)})
                    .success( function( data, status, headers, config ) {
                        $scope.rxChartObject.data = data.array;
                    });
            }
            $scope.rxChartObject = {};
            $scope.rxChartObject.type = "LineChart";
            $scope.rxChartObject.options = {
                "vAxes": {
                    0: {"title": 'Data Rate'},
                    1: {"title": 'Frequency (MHz)'}
                },
                "series": {
                    0: {"targetAxisIndex": 0},
                    1: {"targetAxisIndex": 1}
                },
                "chartArea": {
                    "top": 0, "bottom": "10%",
                    "left": 0, "right": 0
                },
                "legend": {
                    "position": "none"
                },
                "pointSize": 3,
                "vAxis": {
                    "textPosition": "in",
                    "gridlines": {"count": -1}
                },
                "vAxes": {
                    0: {"minValue": 0, "maxValue": 7},
                    1: {"minValue": 868, "maxValue": 869}
                }
            };
            updateData();
            $scope.stopTime = $interval(updateData, 5000);
            $scope.$on('$destroy', function() {
                $interval.cancel($scope.stopTime);
            });
    },
    template: '<div google-chart chart="rxChartObject"></div>'
};}]);

myApp.directive('qgraph', ['$http', '$interval', function($http, $interval) {
return {
    restrict: 'E',
    scope: {
        value: '=',
    },
    link: function($scope) {
            function updateData() {
                $http({method: 'GET', url: '/rxq/'.concat($scope.value)})
                    .success( function( data, status, headers, config ) {
                        $scope.rxqChartObject.data = data.array;
                    });
            }
            $scope.rxqChartObject = {};
            $scope.rxqChartObject.type = "LineChart";
            $scope.rxqChartObject.options = {
                "vAxes": {
                    0: {"title": 'RSSI (dBm)'},
                    1: {"title": 'SNR (dB)'}
                },
                "series": {
                    0: {"targetAxisIndex": 0},
                    1: {"targetAxisIndex": 1}
                },
                "chartArea": {
                    "top": 0, "bottom": "10%",
                    "left": 0, "right": 0
                },
                "legend": {
                    "position": "none"
                },
                "pointSize": 3,
                "vAxis": {
                    "textPosition": "in",
                    "gridlines": {"count": -1}
                },
                "vAxes": {
                    0: {"maxValue": 0},
                    1: {"minValue": 0}
                }
            };
            updateData();
            $scope.stopTime = $interval(updateData, 5000);
            $scope.$on('$destroy', function() {
                $interval.cancel($scope.stopTime);
            });
    },
    template: '<div google-chart chart="rxqChartObject"></div>'
};}]);
