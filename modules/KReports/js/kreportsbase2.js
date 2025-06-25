/**
 * This file is part of KReporter. KReporter is an enhancement developed
 * by Christian Knoll. All rights are (c) 2012 by Christian Knoll
 *
 * This file has been modified by SinergiaTIC in SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU AFFERO GENERAL PUBLIC LICENSE as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU AFFERO GENERAL PUBLIC LICENSE
 * along with this program; if not, see http://www.gnu.org/licenses
 * or write to the Free Software Foundation,Inc., 51 Franklin Street,
 * Fifth Floor, Boston, MA 02110-1301  USA
 *
 * You can contact Christian Knoll at info@kreporter.org
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */

Ext.namespace("K.kreports.whereOperators");
Ext.define("K.kreports.whereOperators.bP", {
    extend: "Ext.data.Model",
    fields: [
        { name: "value", type: "string" },
        { name: "text", type: "string" },
    ],
});
K.kreports.whereOperators.ay = new Ext.data.Store({
    model: K.kreports.whereOperators.bP,
    proxy: { type: "ajax", url: "index.php?module=KReports&action=get_enum&to_pdf=true", reader: { type: "json" } },
});
Ext.define("K.kreports.whereOperators.parentFieldsModel", {
    extend: "Ext.data.Model",
    fields: [
        { name: "field", type: "string" },
        { name: "description", type: "string" },
    ],
});
K.kreports.whereOperators.parentFieldsStore = new Ext.data.Store({
    model: K.kreports.whereOperators.parentFieldsModel,
    proxy: { type: "ajax", url: "index.php?module=KReports&action=get_modulefields&to_pdf=true", reader: { type: "json" } },
});
Ext.define("K.kreports.whereOperators.whereOperatorsModel", {
    extend: "Ext.data.Model",
    fields: [{ name: "operator" }, { name: "values" }, { name: "display" }],
});
K.kreports.whereOperators.whereOperatorsStore = new Ext.data.Store({
    model: K.kreports.whereOperators.whereOperatorsModel,
    proxy: { type: "ajax", url: "index.php?module=KReports&action=get_whereoperators&to_pdf=true", reader: { type: "json" } },
    listeners: {
        load: function () {
            if (K.kreports.integrationpanel != undefined) {
                var publishData = K.kreports.integrationpanel.mainpanel.getPluginData("kpublishing");
                if (publishData !== false && publishData.subpanelModule != undefined && publishData.subpanelModule != "") {
                    K.kreports.whereOperators.whereOperatorsStore.add({
                        operator: "parent_assign",
                        values: 1,
                        display: bi("LBL_OP_PARENT_ASSIGN"),
                    });
                }
            }
        },
    },
});
K.kreports.whereOperators.bh = function (cQ, df) {
    if (typeof kreportoperatorcount[cQ] != undefined) return kreportoperatorcount[cQ];
    else return 0;
};
Ext.define("K.kreports.whereOperators.bC", { extend: "Ext.data.Model", fields: [{ name: "field" }, { name: "description" }] });
K.kreports.whereOperators.aK = new Ext.data.Store({
    model: K.kreports.whereOperators.bC,
    proxy: { type: "ajax", url: "index.php?module=KReports&action=get_wherefunctions&to_pdf=true", reader: { type: "json" } },
});
Ext.define("K.kreports.whereOperators.bA", { extend: "Ext.data.Model", fields: [{ name: "itemid" }, { name: "itemtext" }] });
K.kreports.whereOperators.bz = new Ext.data.Store({
    model: K.kreports.whereOperators.bA,
    proxy: {
        type: "ajax",
        url: "index.php?module=KReports&action=get_autocompletevalues&to_pdf=true",
        reader: { type: "json", root: "data", totalProperty: "total" },
    },
});
K.kreports.whereOperators.editorWindow = new Ext.window.Window({
    modal: true,
    height: 200,
    width: 400,
    layout: "fit",
    record: null,
    editedField: null,
    editedFieldText: null,
    title: "Editor",
    closeAction: "hide",
    items: [{ xtype: "textarea" }],
    buttons: [
        {
            text: "OK",
            handler: function () {
                if (K.kreports.whereOperators.editorWindow.items.items[0].getValue() != "")
                    K.kreports.whereOperators.editorWindow.record.set(
                        K.kreports.whereOperators.editorWindow.editedField,
                        K.kreports.encode64(K.kreports.whereOperators.editorWindow.items.items[0].getValue())
                    );
                else K.kreports.whereOperators.editorWindow.record.set(K.kreports.whereOperators.editorWindow.editedField, "");
                K.kreports.whereOperators.editorWindow.close();
            },
        },
        {
            text: "Cancel",
            handler: function () {
                K.kreports.whereOperators.editorWindow.close();
            },
        },
    ],
    listeners: {
        show: function () {
            this.items.items[0].setValue(K.kreports.decode64(this.record.get(this.editedField)));
            this.setTitle(this.record.get("name") + " - " + this.editedFieldText);
        },
    },
});
var currentEditedRecord;
var currentEditedGrid;
var currentEditedColumn;
var aF = new Object();
var bM = function (e, bH) {
    if (e.column.dataIndex == "customsqlfunction") {
        K.kreports.whereOperators.editorWindow.record = e.record;
        K.kreports.whereOperators.editorWindow.editedField = e.column.dataIndex;
        K.kreports.whereOperators.editorWindow.editedFieldText = e.column.text;
        K.kreports.whereOperators.editorWindow.show();
        return false;
        if (e.value != undefined && e.value != "") {
        }
    }
    if (e.column.id == "onoffswitch") {
        if (e.record.data.usereditable == "yo1" || e.record.data.usereditable == "yo2") {
            e.column.setEditor(
                new Ext.form.field.ComboBox({
                    typeAhead: true,
                    triggerAction: "all",
                    lazyRender: true,
                    queryMode: "local",
                    store: new Ext.data.ArrayStore({
                        id: 0,
                        fields: ["value", "text"],
                        data: [
                            ["yo1", bi("LBL_ONOFF_YO1")],
                            ["yo2", bi("LBL_ONOFF_YO2")],
                        ],
                    }),
                    displayField: "text",
                    valueField: "value",
                })
            );
        } else e.column.setEditor(null);
    }
    if (e.column.id == "value" || e.column.id == "valueto") {
        var cf = K.kreports.whereOperators.bh(e.record.get("operator"), e.record.get("type"));
        if (!K.kreports.EditView) {
            if (e.record.get("usereditable") == "yo1" || e.record.get("usereditable") == "yo2") cf = 0;
        }
        if (cf === 0 || (e.column.id == "valueto" && cf != 2)) {
            e.cancel = true;
            return false;
        } else {
            switch (e.record.data.operator) {
                case "autocomplete":
                    K.kreports.whereOperators.bz.getProxy().extraParams.path = e.record.data.path;
                    K.kreports.whereOperators.aE = new Ext.form.ComboBox({
                        store: K.kreports.whereOperators.bz,
                        valueField: "itemid",
                        displayField: "itemtext",
                        typeAhead: true,
                        mode: "remote",
                        pageSize: 25,
                        listConfig: { minWidth: 250 },
                        minChars: 1,
                        triggerAction: "all",
                        forceSelection: true,
                        selectOnFocus: true,
                    });
                    e.column.setEditor(K.kreports.whereOperators.aE);
                    break;
                case "parent_assign":
                    var publishData = K.kreports.integrationpanel.mainpanel.getPluginData("kpublishing");
                    if (publishData !== false && publishData.subpanelModule != undefined && publishData.subpanelModule != "") {
                        K.kreports.whereOperators.parentFieldsStore.removeAll();
                        K.kreports.whereOperators.parentFieldsStore.getProxy().extraParams.inputmodule = publishData.subpanelModule;
                        K.kreports.whereOperators.parentFieldsStore.load();
                        K.kreports.whereOperators.bx = new Ext.form.ComboBox({
                            store: K.kreports.whereOperators.parentFieldsStore,
                            valueField: "field",
                            displayField: "description",
                            typeAhead: true,
                            queryMode: "local",
                            triggerAction: "all",
                            editable: false,
                            selectOnFocus: true,
                            listWidth: 200,
                        });
                        e.column.setEditor(K.kreports.whereOperators.bx);
                    } else {
                        e.cancel = true;
                        return false;
                    }
                    break;
                case "function":
                    K.kreports.whereOperators.aK.removeAll();
                    K.kreports.whereOperators.aK.load();
                    K.kreports.whereOperators.as = new Ext.form.ComboBox({
                        store: K.kreports.whereOperators.aK,
                        valueField: "field",
                        displayField: "description",
                        typeAhead: true,
                        mode: "local",
                        triggerAction: "all",
                        editable: false,
                        selectOnFocus: true,
                        listWidth: 200,
                    });
                    e.column.setEditor(K.kreports.whereOperators.as);
                    break;
                case "reference":
                    e.column.setEditor(new Ext.form.TextField());
                    break;
                default:
                    switch (e.record.data.type) {
                        case "datetimecombo":
                        case "datetime":
                            switch (e.record.data.operator) {
                                // STIC-Custom 20211104 AAM - Adding operators "after/before N days"  functionality
                                // STIC#458
                                case "beforendays":
                                case "lastndays":
                                case "lastnfdays":
                                case "lastnweeks":
                                case "notlastnweeks":
                                case "lastnfweeks":
                                case "lastnfmonth":
                                // STIC-Custom 20211104 AAM - Adding operators "after/before N days"  functionality
                                // STIC#458
                                case "afterndays":
                                case "nextndays":
                                case "nextnmonth":
                                case "nextnweeks":
                                case "notnextnweeks":
                                case "betwndays":
                                    e.column.setEditor(new Ext.form.NumberField());
                                    break;
                                default:
                                    if (
                                        K.kreports.EditView &&
                                        (e.record.data.operator == "lastnddays" ||
                                            e.record.data.operator == "nextnddays" ||
                                            e.record.data.operator == "betwnddays")
                                    )
                                        e.column.setEditor(new Ext.form.field.Number());
                                    else {
                                        e.column.setEditor(new Ext.form.field.Number());
                                        currentEditedRecord = e;
                                        currentEditedGrid = bH;
                                        currentEditedColum = e.column.id;
                                        if (e.record.get(currentEditedColum) != "") {
                                            var dateArray = e.record.get(currentEditedColum).split(" ");
                                            var timeArray = dateArray[1].split(":");
                                        } else {
                                            var dateArray = ["", "00:00:00"];
                                            var timeArray = dateArray[1].split(":");
                                        }
                                        var cu = new Ext.form.DateField({
                                            fieldLabel: bi("LBL_DATETIMEPICKER_DATE"),
                                            width: 130,
                                            value: dateArray[0],
                                            format: cal_date_format.replace(/%/g, ""),
                                        });
                                        var cZ = new Ext.form.NumberField({
                                            name: "hours",
                                            xtype: "numberfield",
                                            value: timeArray[0],
                                            minValue: 0,
                                            maxValue: 23,
                                            allowNegative: false,
                                            listeners: {
                                                invalid: function () {
                                                    if (this.getValue() > 23) this.setValue(23);
                                                    if (this.getValue() < 0) this.setValue(0);
                                                },
                                            },
                                            width: 40,
                                        });
                                        var ck = new Ext.form.NumberField({
                                            name: "minutes",
                                            xtype: "numberfield",
                                            value: timeArray[1],
                                            minValue: 0,
                                            maxValue: 59,
                                            setValue: function (v) {
                                                v = this.fixPrecision(v);
                                                v = Ext.isNumber(v) ? v : parseFloat(String(v).replace(this.decimalSeparator, "."));
                                                v = isNaN(v) ? "" : String(v).replace(".", this.decimalSeparator);
                                                if (v < 10) v = "0" + v;
                                                return Ext.form.NumberField.superclass.setValue.call(this, v);
                                            },
                                            allowNegative: false,
                                            getTimeValue: function () {
                                                if (this.getValue() > 10) return this.getValue();
                                                else return "0" + this.getValue();
                                            },
                                            listeners: {
                                                invalid: function () {
                                                    if (this.getValue() > 59) this.setValue(59);
                                                    if (this.getValue() < 0) this.setValue(0);
                                                },
                                            },
                                            width: 40,
                                        });
                                        var ci = new Ext.form.NumberField({
                                            name: "seconds",
                                            xtype: "numberfield",
                                            value: timeArray[2],
                                            minValue: 0,
                                            maxValue: 59,
                                            allowNegative: false,
                                            setValue: function (v) {
                                                v = this.fixPrecision(v);
                                                v = Ext.isNumber(v) ? v : parseFloat(String(v).replace(this.decimalSeparator, "."));
                                                v = isNaN(v) ? "" : String(v).replace(".", this.decimalSeparator);
                                                if (v < 10) v = "0" + v;
                                                return Ext.form.NumberField.superclass.setValue.call(this, v);
                                            },
                                            getTimeValue: function () {
                                                if (this.getValue() > 10) return this.getValue();
                                                else return "0" + this.getValue();
                                            },
                                            listeners: {
                                                invalid: function () {
                                                    if (this.getValue() > 59) this.setValue(59);
                                                    if (this.getValue() < 0) this.setValue(0);
                                                },
                                            },
                                            width: 40,
                                        });
                                        K.kreports.whereOperators.datepicker = new Ext.Window({
                                            title: bi("LBL_DATETIMEPICKER_POPUP_TITLE"),
                                            layout: "form",
                                            drggable: true,
                                            x: aF[0],
                                            y: aF[1],
                                            labelWidth: 50,
                                            width: 250,
                                            closeAction: "hide",
                                            draggable: true,
                                            modal: true,
                                            items: [
                                                cu,
                                                {
                                                    xtype: "fieldcontainer",
                                                    layout: "hbox",
                                                    fieldLabel: "Time",
                                                    combineErrors: false,
                                                    items: [
                                                        cZ,
                                                        { xtype: "displayfield", width: 3, value: ":" },
                                                        ck,
                                                        { xtype: "displayfield", width: 3, value: ":" },
                                                        ci,
                                                    ],
                                                },
                                            ],
                                            buttons: [
                                                {
                                                    text: bi("LBL_DATETIMEPICKER_CANCEL_BUTTON"),
                                                    handler: function () {
                                                        K.kreports.whereOperators.datepicker.close();
                                                    },
                                                },
                                                {
                                                    text: bi("LBL_DATETIMEPICKER_CLOSE_BUTTON"),
                                                    handler: function () {
                                                        currentEditedRecord.record.set(
                                                            currentEditedColum,
                                                            Ext.Date.format(cu.getValue(), cal_date_format.replace(/%/g, "")) +
                                                                " " +
                                                                cZ.getValue() +
                                                                ":" +
                                                                ck.getTimeValue() +
                                                                ":" +
                                                                ci.getTimeValue()
                                                        );
                                                        currentEditedRecord.record.set(
                                                            currentEditedColum + "key",
                                                            Ext.Date.format(cu.getValue(), "Y-m-d") +
                                                                " " +
                                                                cZ.getValue() +
                                                                ":" +
                                                                ck.getTimeValue() +
                                                                ":" +
                                                                ci.getTimeValue()
                                                        );
                                                        currentEditedGrid.editingPlugin.fireEvent("edit", null, currentEditedRecord);
                                                        K.kreports.whereOperators.datepicker.close();
                                                    },
                                                },
                                            ],
                                        });
                                        K.kreports.whereOperators.datepicker.show(this);
                                    }
                                    break;
                            }
                            break;
                        case "date":
                            switch (e.record.data.operator) {
                                // STIC-Custom 20211104 AAM - Adding operators "after/before N days"  functionality
                                // STIC#458
                                case "beforendays":
                                case "lastndays":
                                case "lastnfdays":
                                case "lastnweeks":
                                case "notlastnweeks":
                                case "lastnfweeks":
                                case "lastnfmonth":
                                // STIC-Custom 20211104 AAM - Adding operators "after/before N days"  functionality
                                // STIC#458
                                case "afterndays":
                                case "nextndays":
                                case "nextnmonth":
                                case "nextnweeks":
                                case "notnextnweeks":
                                case "betwndays":
                                    e.column.setEditor(new Ext.form.NumberField());
                                    break;
                                case "lastnddays":
                                case "nextnddays":
                                case "betwnddays":
                                    if (K.kreports.EditView) e.column.setEditor(new Ext.form.NumberField());
                                    else
                                        e.column.setEditor(
                                            new Ext.form.DateField({
                                                editable: false,
                                                value: e.value,
                                                format: cal_date_format.replace(/%/g, ""),
                                            })
                                        );
                                    break;
                                default:
                                    e.column.setEditor(
                                        new Ext.form.DateField({ editable: false, format: cal_date_format.replace(/%/g, "") })
                                    );
                                    break;
                            }
                            break;
                        case "user_name":
                        case "assigned_user_name":
                        case "enum":
                        case "parent_type":
                        case "radioenum":
                        case "dynamicenum":
                        case "multienum":
                            if (
                                e.record.data.operator == "starts" ||
                                e.record.data.operator == "notstarts" ||
                                e.record.data.operator == "contains" ||
                                e.record.data.operator == "notcontains"
                            ) {
                                e.column.setEditor(new Ext.form.TextField());
                            } else {
                                K.kreports.whereOperators.ay.removeAll();
                                K.kreports.whereOperators.ay.getProxy().extraParams.path = e.record.data.path;
                                K.kreports.whereOperators.ay.load();
                                switch (e.record.data.operator) {
                                    case "oneof":
                                    case "oneofnot":
                                    case "oneofnotornull":
                                        e.column.setEditor(
                                            new Ext.form.ComboBox({
                                                typeAhead: false,
                                                editable: false,
                                                triggerAction: "all",
                                                lazyRender: true,
                                                multiSelect: true,
                                                queryMode: "local",
                                                store: K.kreports.whereOperators.ay,
                                                displayField: "text",
                                                valueField: "value",
                                                listConfig: { minWidth: 200, resizable: true },
                                            })
                                        );
                                        break;
                                    // STIC-Custom EPS 20241220 - equals & notequal must show multi-select also
                                    // https://github.com/SinergiaTIC/SinergiaCRM/pull/523
                                    case "equals":
                                    case "notequal":
                                        if (e.record.data.type == 'multienum'){
                                            e.column.setEditor(
                                                new Ext.form.ComboBox({
                                                    typeAhead: false,
                                                    editable: false,
                                                    triggerAction: "all",
                                                    lazyRender: true,
                                                    multiSelect: true,
                                                    queryMode: "local",
                                                    store: K.kreports.whereOperators.ay,
                                                    displayField: "text",
                                                    valueField: "value",
                                                    listConfig: { minWidth: 200, resizable: true },
                                                })
                                            );
                                        }
                                        else {
                                            e.column.setEditor(
                                                new Ext.form.ComboBox({
                                                    typeAhead: true,
                                                    editable: true,
                                                    forceSelection: true,
                                                    triggerAction: "all",
                                                    lazyRender: true,
                                                    multiSelect: false,
                                                    queryMode: "local",
                                                    store: K.kreports.whereOperators.ay,
                                                    displayField: "text",
                                                    valueField: "value",
                                                    listConfig: { minWidth: 200, resizable: true },
                                                })
                                            );
                                        }
                                        break;
                                    // END STIC-Custom
                                    default:
                                        e.column.setEditor(
                                            new Ext.form.ComboBox({
                                                typeAhead: true,
                                                editable: true,
                                                forceSelection: true,
                                                triggerAction: "all",
                                                lazyRender: true,
                                                multiSelect: false,
                                                queryMode: "local",
                                                store: K.kreports.whereOperators.ay,
                                                displayField: "text",
                                                valueField: "value",
                                                listConfig: { minWidth: 200, resizable: true },
                                            })
                                        );
                                        break;
                                }
                            }
                            break;
                        case "bool":
                            e.column.setEditor(
                                new Ext.form.ComboBox({
                                    typeAhead: true,
                                    triggerAction: "all",
                                    lazyRender: true,
                                    queryMode: "local",
                                    store: new Ext.data.ArrayStore({
                                        id: 0,
                                        fields: ["value", "text"],
                                        data: [
                                            ["1", bi("LBL_BOOL_1")],
                                            ["0", bi("LBL_BOOL_0")],
                                        ],
                                    }),
                                    displayField: "text",
                                    valueField: "value",
                                })
                            );
                            break;
                        default:
                            e.column.setEditor(new Ext.form.TextField());
                            break;
                    }
                    break;
            }
        }
    }
    if (e.column.id == "operator") {
        if (e.record.data.usereditable == "yes" || K.kreports.EditView) {
            K.kreports.whereOperators.whereOperatorsStore.removeAll();
            K.kreports.whereOperators.whereOperatorsStore.getProxy().extraParams.path = e.record.data.path;
            K.kreports.whereOperators.whereOperatorsStore.getProxy().extraParams.editview = K.kreports.EditView;
            K.kreports.whereOperators.whereOperatorsStore.load();
            e.column.setEditor(
                new Ext.form.field.ComboBox({
                    typeAhead: true,
                    triggerAction: "all",
                    lazyRender: true,
                    queryMode: "local",
                    editable: true,
                    store: K.kreports.whereOperators.whereOperatorsStore,
                    displayField: "display",
                    valueField: "operator",
                })
            );
        } else e.column.setEditor(null);
    }
};
var aL = function (e) {
    if (e.column.id == "value" || e.column.id == "valueto") {
        switch (e.record.data.operator) {
            case "autocomplete":
                e.record.set(e.column.id + "key", e.value);
                e.record.set(
                    e.column.id,
                    K.kreports.whereOperators.bz.getAt(K.kreports.whereOperators.bz.find("itemid", e.value)).get("itemtext")
                );
                break;
            case "parent_assign":
                e.record.set(e.column.id + "key", e.value);
                e.record.set(
                    e.column.id,
                    K.kreports.whereOperators.bt.getAt(K.kreports.whereOperators.bt.find("field", e.value)).get("description")
                );
                break;
            case "function":
                e.record.set(e.column.id + "key", e.value);
                e.record.set(
                    e.column.id,
                    K.kreports.whereOperators.aK.getAt(K.kreports.whereOperators.aK.find("field", e.value)).get("description")
                );
                break;
            case "reference":
                break;
            default:
                switch (e.record.data.type) {
                    case "datetime":
                    case "datetimecombo":
                        break;
                    case "date":
                        switch (e.record.data.operator) {
                            // STIC-Custom 20211104 AAM - Adding operators "after/before N days"  functionality
                            // STIC#458
                            case "beforendays":
                            case "lastndays":
                            case "lastnfdays":
                            case "lastnweeks":
                            case "notlastnweeks":
                            case "lastnfweeks":
                            case "lastnfmonth":
                            // STIC-Custom 20211104 AAM - Adding operators "after/before N days"  functionality
                            // STIC#458
                            case "afterndays":
                            case "nextndays":
                            case "nextnweeks":
                            case "nextnmonth":
                            case "notnextnweeks":
                            case "betwndays":
                                break;
                            case "lastnddays":
                            case "nextnddays":
                            case "betwnddays":
                                if (!K.kreports.EditView) {
                                    e.record.set(e.column.id + "key", Ext.Date.format(e.value, "Y-m-d"));
                                    e.record.set(e.column.id, Ext.Date.format(e.value, cal_date_format.replace(/%/g, "")));
                                }
                                break;
                            default:
                                e.record.set(e.column.id + "key", Ext.Date.format(e.value, "Y-m-d"));
                                e.record.set(e.column.id, Ext.Date.format(e.value, cal_date_format.replace(/%/g, "")));
                                break;
                        }
                        break;
                    case "user_name":
                    case "assigned_user_name":
                    case "enum":
                    case "radioenum":
                    case "dynamicenum":
                    case "parent_type":
                    case "multienum":
                        switch (e.record.data.operator) {
                            case "oneof":
                            case "oneofnot":
                            case "oneofnotornull":
                                var cR = e.column.getEditor().getStore();
                                var dc = "";
                                for (var i = 0; i < e.value.length; i++) {
                                    if (i > 0) dc += ", ";
                                    var cr = cR.findRecord("value", e.value[i]);
                                    if (cr) dc += cr.get("text");
                                    else dc += e.value[i];
                                }
                                e.record.set(e.column.id, dc);
                                e.record.set(e.column.id + "key", e.value);
                                break;
                            case "starts":
                            case "notstarts":
                            case "contains":
                            case "notcontains":
                                break;
                            // STIC-Custom EPS 20241220 - equals & notequal must show multi-select also
                            // https://github.com/SinergiaTIC/SinergiaCRM/pull/523
                            case "equals":
                            case "notequal":
                                if (e.record.data.type == 'multienum') {
                                    var cR = e.column.getEditor().getStore();
                                    var dc = "";
                                    for (var i = 0; i < e.value.length; i++) {
                                        if (i > 0) dc += ", ";
                                        var cr = cR.findRecord("value", e.value[i]);
                                        if (cr) dc += cr.get("text");
                                        else dc += e.value[i];
                                    }
                                    e.record.set(e.column.id, dc);
                                    e.record.set(e.column.id + "key", e.value);
                                }
                                else {
                                    e.record.set(e.column.id + "key", e.value);
                                    e.record.set(
                                        e.column.id,
                                        K.kreports.whereOperators.ay.getAt(K.kreports.whereOperators.ay.find("value", e.value)).get("text")
                                    );   
                                }
                                break;
                            // END STIC-Custom    
                            default:
                                e.record.set(e.column.id + "key", e.value);
                                e.record.set(
                                    e.column.id,
                                    // STIC-Custom 20240117 EPS - Wrong literal was being displayed when keys begin with same root. Changed find to search for exact match
                                    // https://github.com/SinergiaTIC/SinergiaCRM/pull/547
                                    // More info at https://cdn.sencha.com/ext/gpl/4.2.1/docs/#!/api/Ext.data.Store
                                    // K.kreports.whereOperators.ay.getAt(K.kreports.whereOperators.ay.find("value", e.value)).get("text")
                                    K.kreports.whereOperators.ay.getAt(K.kreports.whereOperators.ay.findExact("value", e.value)).get("text")
                                    // END STIC-Custom
                                );
                                break;
                        }
                        break;
                    case "bool":
                        e.record.set(e.column.id + "key", e.value);
                        e.record.set(e.column.id, bi("LBL_BOOL_" + e.value));
                        break;
                    default:
                        break;
                }
                break;
        }
    }
    if (e.column.id == "operator") {
        e.record.set("value", "");
        e.record.set("valueto", "");
        e.record.set("valuekey", "");
        e.record.set("valuetokey", "");
    }
};
