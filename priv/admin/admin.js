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
            .validation({ pattern: '[A-Za-z0-9]{8}' })
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
            .defaultValue(0)
    ]);
    links.editionView().fields(
        links.creationView().fields().concat([
        nga.field('test', 'template').label('Ignore me, I am test')
            .template('<p class="readonly half">{{value.device}}</p><ma-input-field class="half" type="number" field="::field" value="value.desired"></ma-input-field>'),
        nga.field('devaddr', 'template').label('RX Quality')
            .template('<graph value="value"></graph>'),
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

myApp.directive('graph', ['$http', function($http) {
return {
    restrict: 'E',
    scope: {
        value: '=',
    },
    link: function($scope) {
            $scope.myChartObject = {};
            $scope.myChartObject.type = "LineChart";
            $http({method: 'GET', url: '/rxq/'.concat($scope.value)})
                .success( function( data, status, headers, config ) {
                    $scope.myChartObject.data = data.array;
                });
            $scope.myChartObject.options = {
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
    },
    template: '<div google-chart chart="myChartObject"></div>'
};}]);
