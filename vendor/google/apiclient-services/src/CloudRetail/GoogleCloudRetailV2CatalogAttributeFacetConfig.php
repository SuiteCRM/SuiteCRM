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

class GoogleCloudRetailV2CatalogAttributeFacetConfig extends \Google\Collection
{
  protected $collection_key = 'mergedFacetValues';
  protected $facetIntervalsType = GoogleCloudRetailV2Interval::class;
  protected $facetIntervalsDataType = 'array';
  protected $ignoredFacetValuesType = GoogleCloudRetailV2CatalogAttributeFacetConfigIgnoredFacetValues::class;
  protected $ignoredFacetValuesDataType = 'array';
  protected $mergedFacetType = GoogleCloudRetailV2CatalogAttributeFacetConfigMergedFacet::class;
  protected $mergedFacetDataType = '';
  protected $mergedFacetValuesType = GoogleCloudRetailV2CatalogAttributeFacetConfigMergedFacetValue::class;
  protected $mergedFacetValuesDataType = 'array';
  protected $rerankConfigType = GoogleCloudRetailV2CatalogAttributeFacetConfigRerankConfig::class;
  protected $rerankConfigDataType = '';

  /**
   * @param GoogleCloudRetailV2Interval[]
   */
  public function setFacetIntervals($facetIntervals)
  {
    $this->facetIntervals = $facetIntervals;
  }
  /**
   * @return GoogleCloudRetailV2Interval[]
   */
  public function getFacetIntervals()
  {
    return $this->facetIntervals;
  }
  /**
   * @param GoogleCloudRetailV2CatalogAttributeFacetConfigIgnoredFacetValues[]
   */
  public function setIgnoredFacetValues($ignoredFacetValues)
  {
    $this->ignoredFacetValues = $ignoredFacetValues;
  }
  /**
   * @return GoogleCloudRetailV2CatalogAttributeFacetConfigIgnoredFacetValues[]
   */
  public function getIgnoredFacetValues()
  {
    return $this->ignoredFacetValues;
  }
  /**
   * @param GoogleCloudRetailV2CatalogAttributeFacetConfigMergedFacet
   */
  public function setMergedFacet(GoogleCloudRetailV2CatalogAttributeFacetConfigMergedFacet $mergedFacet)
  {
    $this->mergedFacet = $mergedFacet;
  }
  /**
   * @return GoogleCloudRetailV2CatalogAttributeFacetConfigMergedFacet
   */
  public function getMergedFacet()
  {
    return $this->mergedFacet;
  }
  /**
   * @param GoogleCloudRetailV2CatalogAttributeFacetConfigMergedFacetValue[]
   */
  public function setMergedFacetValues($mergedFacetValues)
  {
    $this->mergedFacetValues = $mergedFacetValues;
  }
  /**
   * @return GoogleCloudRetailV2CatalogAttributeFacetConfigMergedFacetValue[]
   */
  public function getMergedFacetValues()
  {
    return $this->mergedFacetValues;
  }
  /**
   * @param GoogleCloudRetailV2CatalogAttributeFacetConfigRerankConfig
   */
  public function setRerankConfig(GoogleCloudRetailV2CatalogAttributeFacetConfigRerankConfig $rerankConfig)
  {
    $this->rerankConfig = $rerankConfig;
  }
  /**
   * @return GoogleCloudRetailV2CatalogAttributeFacetConfigRerankConfig
   */
  public function getRerankConfig()
  {
    return $this->rerankConfig;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudRetailV2CatalogAttributeFacetConfig::class, 'Google_Service_CloudRetail_GoogleCloudRetailV2CatalogAttributeFacetConfig');
