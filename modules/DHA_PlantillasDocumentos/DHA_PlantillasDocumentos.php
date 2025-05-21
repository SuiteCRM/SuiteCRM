<?PHP
/**
 * This file is part of Mail Merge Reports by Izertis.
 * Copyright (C) 2015 Izertis. 
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
 * You can contact Izertis at email address info@izertis.com.
 */
// NOTA: Se crean clases condicionales para evitar problemas con el modo estricto de PHP 7

require_once('modules/DHA_PlantillasDocumentos/DHA_PlantillasDocumentosParent.php');

global $sugar_version;
$sugar_7 = version_compare($sugar_version, '7.0.0', '>=');


if ($sugar_7) {
#[\AllowDynamicProperties]
class DHA_PlantillasDocumentos extends DHA_PlantillasDocumentosParent {

   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function populateFromRow(array $row, $convert = false) {
       
      $row = parent::populateFromRow($row, $convert);

      if (!empty($this->document_name) && empty($this->name)) {
         $this->name = $this->document_name;
      }

      return $row;
   }

}
}


if (!$sugar_7) {
#[\AllowDynamicProperties]
class DHA_PlantillasDocumentos extends DHA_PlantillasDocumentosParent {

   ///////////////////////////////////////////////////////////////////////////////////////////////////
   function populateFromRow($row) {

      parent::populateFromRow($row);

      if (!empty($this->document_name) && empty($this->name)) {
         $this->name = $this->document_name;
      }
   }

}
}


?>