<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
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
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */


require_once 'modules/ModuleBuilder/parsers/constants.php' ;

#[\AllowDynamicProperties]
class RelationshipFactory
{

    /*
     * Construct a new relationship of type as provided by the $definition
     * @param array $definition Complete definition of the relationship, as specified by AbstractRelationship::keys
     */
    public static function newRelationship($definition = array())
    {
        // handle the case where a relationship_type is not provided - set it to Many-To-Many as this was the usual type in ModuleBuilder
        if (! isset($definition [ 'relationship_type' ])) {
            $definition [ 'relationship_type' ] = MB_MANYTOMANY ;
        }

        if (!empty($definition['for_activities']) && $definition['for_activities'] == true) {
            require_once 'modules/ModuleBuilder/parsers/relationships/ActivitiesRelationship.php';
            return new ActivitiesRelationship($definition);
        }

        switch (strtolower($definition [ 'relationship_type' ])) {
            case strtolower(MB_ONETOONE):
                require_once 'modules/ModuleBuilder/parsers/relationships/OneToOneRelationship.php' ;
                return new OneToOneRelationship($definition) ;

            case strtolower(MB_ONETOMANY):
                require_once 'modules/ModuleBuilder/parsers/relationships/OneToManyRelationship.php' ;
                return new OneToManyRelationship($definition) ;

            case strtolower(MB_MANYTOONE):
                require_once 'modules/ModuleBuilder/parsers/relationships/ManyToOneRelationship.php' ;
                return new ManyToOneRelationship($definition) ;

            // default case is Many-To-Many as this was the only type ModuleBuilder could create and so much of the MB code assumes Many-To-Many
            default:
                $definition [ 'relationship_type' ] = MB_MANYTOMANY ;
                require_once 'modules/ModuleBuilder/parsers/relationships/ManyToManyRelationship.php' ;
                return new ManyToManyRelationship($definition) ;
        }
    }
}
