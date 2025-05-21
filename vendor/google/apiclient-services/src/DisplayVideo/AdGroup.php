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

class AdGroup extends \Google\Model
{
  /**
   * @var string
   */
  public $adGroupFormat;
  /**
   * @var string
   */
  public $adGroupId;
  /**
   * @var string
   */
  public $advertiserId;
  protected $bidStrategyType = BiddingStrategy::class;
  protected $bidStrategyDataType = '';
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $entityStatus;
  /**
   * @var string
   */
  public $lineItemId;
  /**
   * @var string
   */
  public $name;
  protected $productFeedDataType = ProductFeedData::class;
  protected $productFeedDataDataType = '';
  protected $targetingExpansionType = TargetingExpansionConfig::class;
  protected $targetingExpansionDataType = '';

  /**
   * @param string
   */
  public function setAdGroupFormat($adGroupFormat)
  {
    $this->adGroupFormat = $adGroupFormat;
  }
  /**
   * @return string
   */
  public function getAdGroupFormat()
  {
    return $this->adGroupFormat;
  }
  /**
   * @param string
   */
  public function setAdGroupId($adGroupId)
  {
    $this->adGroupId = $adGroupId;
  }
  /**
   * @return string
   */
  public function getAdGroupId()
  {
    return $this->adGroupId;
  }
  /**
   * @param string
   */
  public function setAdvertiserId($advertiserId)
  {
    $this->advertiserId = $advertiserId;
  }
  /**
   * @return string
   */
  public function getAdvertiserId()
  {
    return $this->advertiserId;
  }
  /**
   * @param BiddingStrategy
   */
  public function setBidStrategy(BiddingStrategy $bidStrategy)
  {
    $this->bidStrategy = $bidStrategy;
  }
  /**
   * @return BiddingStrategy
   */
  public function getBidStrategy()
  {
    return $this->bidStrategy;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param string
   */
  public function setEntityStatus($entityStatus)
  {
    $this->entityStatus = $entityStatus;
  }
  /**
   * @return string
   */
  public function getEntityStatus()
  {
    return $this->entityStatus;
  }
  /**
   * @param string
   */
  public function setLineItemId($lineItemId)
  {
    $this->lineItemId = $lineItemId;
  }
  /**
   * @return string
   */
  public function getLineItemId()
  {
    return $this->lineItemId;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param ProductFeedData
   */
  public function setProductFeedData(ProductFeedData $productFeedData)
  {
    $this->productFeedData = $productFeedData;
  }
  /**
   * @return ProductFeedData
   */
  public function getProductFeedData()
  {
    return $this->productFeedData;
  }
  /**
   * @param TargetingExpansionConfig
   */
  public function setTargetingExpansion(TargetingExpansionConfig $targetingExpansion)
  {
    $this->targetingExpansion = $targetingExpansion;
  }
  /**
   * @return TargetingExpansionConfig
   */
  public function getTargetingExpansion()
  {
    return $this->targetingExpansion;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AdGroup::class, 'Google_Service_DisplayVideo_AdGroup');
