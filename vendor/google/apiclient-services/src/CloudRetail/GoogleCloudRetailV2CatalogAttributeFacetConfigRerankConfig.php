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

namespace Google\Service\CloudRetail;

class GoogleCloudRetailV2CatalogAttributeFacetConfigRerankConfig extends \Google\Collection
{
  protected $collection_key = 'facetValues';
  /**
   * @var string[]
   */
  public $facetValues;
  /**
   * @var bool
   */
  public $rerankFacet;

  /**
   * @param string[]
   */
  public function setFacetValues($facetValues)
  {
    $this->facetValues = $facetValues;
  }
  /**
   * @return string[]
   */
  public function getFacetValues()
  {
    return $this->facetValues;
  }
  /**
   * @param bool
   */
  public function setRerankFacet($rerankFacet)
  {
    $this->rerankFacet = $rerankFacet;
  }
  /**
   * @return bool
   */
  public function getRerankFacet()
  {
    return $this->rerankFacet;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudRetailV2CatalogAttributeFacetConfigRerankConfig::class, 'Google_Service_CloudRetail_GoogleCloudRetailV2CatalogAttributeFacetConfigRerankConfig');
