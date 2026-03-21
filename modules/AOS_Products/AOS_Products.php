<?php
/**
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2011 - 2025 SuiteCRM Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUITECRM, SUITECRM DISCLAIMS THE
 * WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License
 * version 3, these Appropriate Legal Notices must retain the display of the
 * "Supercharged by SuiteCRM" logo. If the display of the logos is not reasonably
 * feasible for technical reasons, the Appropriate Legal Notices must display
 * the words "Supercharged by SuiteCRM".
 */

/**
 * THIS CLASS IS FOR DEVELOPERS TO MAKE CUSTOMIZATIONS IN
 */
require_once('modules/AOS_Products/AOS_Products_sugar.php');
#[\AllowDynamicProperties]
class AOS_Products extends AOS_Products_sugar
{
    public function __construct()
    {
        parent::__construct();
    }



    public function getGUID()
    {
        if (function_exists('com_create_guid')) {
            return com_create_guid();
        }
        mt_srand((double)microtime()*10000);//optional for php 4.2.0 and up.
        $charid = strtoupper(md5(uniqid(mt_rand(), true)));
        $hyphen = chr(45);// "-"
        $uuid = substr($charid, 0, 8).$hyphen
                .substr($charid, 8, 4).$hyphen
                .substr($charid, 12, 4).$hyphen
                .substr($charid, 16, 4).$hyphen
                .substr($charid, 20, 12);
        return $uuid;
    }

    public function save($check_notify = false)
    {
        global $sugar_config, $mod_strings;

        if (isset($_POST['deleteAttachment']) && $_POST['deleteAttachment'] == '1') {
            $this->product_image = '';
        }

        require_once('include/upload_file.php');
        $GLOBALS['log']->debug('UPLOADING PRODUCT IMAGE');

        if(!empty($_FILES['uploadimage']['name'])){
            $imageFileName = $_FILES['uploadimage']['name'] ?? '';
            if (!has_valid_image_extension('AOS_Products Uploaded image file: ' . $imageFileName , $imageFileName)) {
                LoggerManager::getLogger()->fatal("AOS_Products save - Invalid image file ext : '$imageFileName'.");
                throw new RuntimeException('Invalid request');
            }
        }

        if (!empty($_FILES['uploadimage']['tmp_name']) && verify_uploaded_image($_FILES['uploadimage']['tmp_name'])) {
            if ($_FILES['uploadimage']['size'] > $sugar_config['upload_maxsize']) {
                die($mod_strings['LBL_IMAGE_UPLOAD_FAIL'] . $sugar_config['upload_maxsize']);
            }
            $prefix_image = $this->getGUID() . '_';
            $this->product_image = $sugar_config['site_url'] . '/' . $sugar_config['upload_dir'] . $prefix_image . $_FILES['uploadimage']['name'];
            move_uploaded_file($_FILES['uploadimage']['tmp_name'], $sugar_config['upload_dir'] . $prefix_image . $_FILES['uploadimage']['name']);
        }

        require_once('modules/AOS_Products_Quotes/AOS_Utils.php');

        perform_aos_save($this);

        return parent::save($check_notify);
    }

    public function getCustomersPurchasedProductsQuery()
    {
        $query = "
 			SELECT * FROM (
 				SELECT
					aos_quotes.*,
					accounts.id AS account_id,
					accounts.name AS billing_account,

					opportunity_id AS opportunity,
					billing_contact_id AS billing_contact,
					'' AS created_by_name,
					'' AS modified_by_name,
					'' AS assigned_user_name
				FROM
					aos_products

				JOIN aos_products_quotes ON aos_products_quotes.product_id = aos_products.id AND aos_products.id = '{$this->id}' AND aos_products_quotes.deleted = 0 AND aos_products.deleted = 0
				JOIN aos_quotes ON aos_quotes.id = aos_products_quotes.parent_id AND aos_quotes.stage = 'Closed Accepted' AND aos_quotes.deleted = 0
				JOIN accounts ON accounts.id = aos_quotes.billing_account_id -- AND accounts.deleted = 0

				GROUP BY aos_quotes.id
			) AS aos_quotes

		";
        return $query;
    }
}
