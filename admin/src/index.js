import React from 'react';
import { render } from 'react-dom';
import { fetchUtils, Admin, Resource } from 'react-admin';
import jsonServerProvider from 'ra-data-json-server';

import MyLayout from './MyLayout';
import Dashboard from './Dashboard';
import { UserList, UserShow, UserCreate, UserEdit } from './Users';

const httpClient = (url, options = {}) => {
    options.credentials = 'include';
    return fetchUtils.fetchJson(url, options);
}
const dataProvider = jsonServerProvider('http://192.168.88.112:8080/api', httpClient);

render(
    <Admin appLayout={MyLayout} dashboard={Dashboard} dataProvider={dataProvider}>
        <Resource name="scopes"/>
        <Resource name="users" list={UserList} show={UserShow} create={UserCreate} edit={UserEdit}/>
    </Admin>,
    document.getElementById('root')
);

