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

namespace Google\Service\DisplayVideo;

class ProductFeedData extends \Google\Collection
{
  protected $collection_key = 'productMatchDimensions';
  /**
   * @var bool
   */
  public $isFeedDisabled;
  protected $productMatchDimensionsType = ProductMatchDimension::class;
  protected $productMatchDimensionsDataType = 'array';
  /**
   * @var string
   */
  public $productMatchType;

  /**
   * @param bool
   */
  public function setIsFeedDisabled($isFeedDisabled)
  {
    $this->isFeedDisabled = $isFeedDisabled;
  }
  /**
   * @return bool
   */
  public function getIsFeedDisabled()
  {
    return $this->isFeedDisabled;
  }
  /**
   * @param ProductMatchDimension[]
   */
  public function setProductMatchDimensions($productMatchDimensions)
  {
    $this->productMatchDimensions = $productMatchDimensions;
  }
  /**
   * @return ProductMatchDimension[]
   */
  public function getProductMatchDimensions()
  {
    return $this->productMatchDimensions;
  }
  /**
   * @param string
   */
  public function setProductMatchType($productMatchType)
  {
    $this->productMatchType = $productMatchType;
  }
  /**
   * @return string
   */
  public function getProductMatchType()
  {
    return $this->productMatchType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ProductFeedData::class, 'Google_Service_DisplayVideo_ProductFeedData');
