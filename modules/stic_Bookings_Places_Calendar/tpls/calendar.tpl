{*
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 *}

<div id='calendar'></div>
{literal}
    <style>
        .option {
            cursor: pointer !important;
        }

        #loading {
            display: none;
            position: absolute;
            top: 10px;
            right: 10px;
        }

        #calendar {
            max-width: 1400px;
            margin: 0 auto;
            /* padding: 0 10px; */
        }

        :root {
            --fc-list-event-hover-bg-color: none;
        }
        .fc-header-toolbar {
            visibility: visible;
            height: auto;
        }

        .fc-col-header-cell {
            display: table-cell !important;
        }

        @media screen and (max-width:767px) {
            .fc-toolbar.fc-header-toolbar {
                flex-direction: column;
                display: contents;
            }

            .fc-toolbar-chunk {
                display: table-row;
                text-align: center;
                padding: 5px 0;
            }
        }

        .fc-button-primary {
            background-color: #353535 !important;
        }

        .fc-button-active {
            background-color: #0A0A0A !important;
        }

        .fc-col-header-cell {
            background-color: #353535 !important;
        }

        .fc-col-header-cell-cushion {
            color: white !important;
        }
    </style>
{/literal}