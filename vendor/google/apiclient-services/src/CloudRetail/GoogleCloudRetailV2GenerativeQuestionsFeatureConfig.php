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

class GoogleCloudRetailV2GenerativeQuestionsFeatureConfig extends \Google\Model
{
  /**
   * @var string
   */
  public $catalog;
  /**
   * @var bool
   */
  public $featureEnabled;
  /**
   * @var int
   */
  public $minimumProducts;

  /**
   * @param string
   */
  public function setCatalog($catalog)
  {
    $this->catalog = $catalog;
  }
  /**
   * @return string
   */
  public function getCatalog()
  {
    return $this->catalog;
  }
  /**
   * @param bool
   */
  public function setFeatureEnabled($featureEnabled)
  {
    $this->featureEnabled = $featureEnabled;
  }
  /**
   * @return bool
   */
  public function getFeatureEnabled()
  {
    return $this->featureEnabled;
  }
  /**
   * @param int
   */
  public function setMinimumProducts($minimumProducts)
  {
    $this->minimumProducts = $minimumProducts;
  }
  /**
   * @return int
   */
  public function getMinimumProducts()
  {
    return $this->minimumProducts;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudRetailV2GenerativeQuestionsFeatureConfig::class, 'Google_Service_CloudRetail_GoogleCloudRetailV2GenerativeQuestionsFeatureConfig');
