/*
 * Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
 * All rights reserved.
 * Distributed under the terms of the MIT License. See the LICENSE file.
 */
var myApp = angular.module('myApp', ['ng-admin']);
myApp.config(['NgAdminConfigurationProvider', function (nga) {
    var admin = nga.application('Server Admin').baseApiUrl('/');

    var applications = nga.entity('applications')
        .identifier(nga.field('name'));
    var devices = nga.entity('devices')
        .identifier(nga.field('deveui'));
    var links = nga.entity('links')
        .identifier(nga.field('devaddr'));

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
            .validation({ pattern: '[A-Za-z0-9]{32}' })
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
    links.editionView().fields(links.creationView().fields());
    // add to the admin application
    admin.addEntity(links);

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
