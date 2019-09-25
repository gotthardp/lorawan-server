import React, { Component } from 'react';
import { connect } from 'react-redux';
import compose from 'recompose/compose';
import { withRouter } from 'react-router-dom';
import {
    translate,
    DashboardMenuItem,
    MenuItemLink,
} from 'react-admin';

import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import { faServer, faUser } from '@fortawesome/free-solid-svg-icons'

import SubMenu from './SubMenu';

class Menu extends Component {
    state = {
        menuCatalog: true,
        menuSales: true,
        menuCustomers: true,
    };

    handleToggle = menu => {
        this.setState(state => ({ [menu]: !state[menu] }));
    };

    render() {
        const { onMenuClick, open, translate } = this.props;
        return (
            <div>
                <DashboardMenuItem onClick={onMenuClick} />
                <SubMenu
                    handleToggle={() => this.handleToggle('menuSales')}
                    isOpen={this.state.menuSales}
                    sidebarIsOpen={open}
                    name="Server"
                    icon={<FontAwesomeIcon icon={faServer}/>}
                >
                    <MenuItemLink to={`/users`} primaryText="Users" leftIcon={<FontAwesomeIcon icon={faUser}/>} onClick={onMenuClick}/>
                </SubMenu>
            </div>
        );
    }
}

const mapStateToProps = state => ({
    open: state.admin.ui.sidebarOpen,
    theme: state.theme,
    locale: state.i18n.locale,
});

const enhance = compose(
    withRouter,
    connect(
        mapStateToProps,
        {}
    ),
    translate
);

export default enhance(Menu);
