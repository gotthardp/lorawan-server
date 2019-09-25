import React from 'react';
import { List, Show, Create, Edit, Datagrid, SimpleShowLayout, SimpleForm, TextField, ChipField, TextInput, DisabledInput, ReferenceArrayInput, AutocompleteArrayInput } from 'react-admin';

export const UserList = (props) => (
    <List {...props}>
        <Datagrid rowClick="edit">
            <TextField label="Name" source="id"/>
            <ChipField source="scopes"/>
        </Datagrid>
    </List>
);

export const UserShow = (props) => (
    <Show {...props}>
        <SimpleShowLayout>
            <TextField source="id" />
            <ChipField source="scopes" />
        </SimpleShowLayout>
    </Show>
);

export const UserCreate = (props) => (
    <Create {...props}>
        <SimpleForm>
            <TextInput label="Name" source="id" />
            <ReferenceArrayInput label="Scopes" source="scopes" reference="scopes">
                <AutocompleteArrayInput />
            </ReferenceArrayInput>
        </SimpleForm>
    </Create>
);

export const UserEdit = (props) => (
    <Edit {...props}>
        <SimpleForm>
            <DisabledInput label="Name" source="id" />
            <ReferenceArrayInput label="Scopes" source="scopes" reference="scopes">
                <AutocompleteArrayInput />
            </ReferenceArrayInput>
        </SimpleForm>
    </Edit>
);
