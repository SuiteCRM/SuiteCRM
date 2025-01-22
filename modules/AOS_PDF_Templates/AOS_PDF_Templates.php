<?php
/**
 * Products, Quotations & Invoices modules.
 * Extensions to SugarCRM
 * @package Advanced OpenSales for SugarCRM
 * @subpackage Products
 * @copyright SalesAgility Ltd http://www.salesagility.com
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
 * @author SalesAgility Ltd <support@salesagility.com>
 */

/**
 * THIS CLASS IS FOR DEVELOPERS TO MAKE CUSTOMIZATIONS IN
 */
require_once('modules/AOS_PDF_Templates/AOS_PDF_Templates_sugar.php');
#[\AllowDynamicProperties]
class AOS_PDF_Templates extends AOS_PDF_Templates_sugar
{
    public function __construct()
    {
        parent::__construct();
    }

    public function cleanBean()
    {
        parent::cleanBean();
        $this->pdfheader = purify_html($this->pdfheader, ['HTML.ForbiddenElements' => ['iframe' => true]]);
        $this->description = purify_html($this->description, ['HTML.ForbiddenElements' => ['iframe' => true]]);
        $this->pdffooter = purify_html($this->pdffooter, ['HTML.ForbiddenElements' => ['iframe' => true]]);
    }
}
