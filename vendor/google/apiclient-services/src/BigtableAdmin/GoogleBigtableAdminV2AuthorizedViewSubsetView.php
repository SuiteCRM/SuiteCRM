<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\BigtableAdmin;

class GoogleBigtableAdminV2AuthorizedViewSubsetView extends \Google\Collection
{
  protected $collection_key = 'rowPrefixes';
  protected $familySubsetsType = GoogleBigtableAdminV2AuthorizedViewFamilySubsets::class;
  protected $familySubsetsDataType = 'map';
  /**
   * @var string[]
   */
  public $rowPrefixes;

  /**
   * @param GoogleBigtableAdminV2AuthorizedViewFamilySubsets[]
   */
  public function setFamilySubsets($familySubsets)
  {
    $this->familySubsets = $familySubsets;
  }
  /**
   * @return GoogleBigtableAdminV2AuthorizedViewFamilySubsets[]
   */
  public function getFamilySubsets()
  {
    return $this->familySubsets;
  }
  /**
   * @param string[]
   */
  public function setRowPrefixes($rowPrefixes)
  {
    $this->rowPrefixes = $rowPrefixes;
  }
  /**
   * @return string[]
   */
  public function getRowPrefixes()
  {
    return $this->rowPrefixes;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleBigtableAdminV2AuthorizedViewSubsetView::class, 'Google_Service_BigtableAdmin_GoogleBigtableAdminV2AuthorizedViewSubsetView');
