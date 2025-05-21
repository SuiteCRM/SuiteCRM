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

namespace Google\Service\Merchant;

class MerchantReview extends \Google\Collection
{
  protected $collection_key = 'customAttributes';
  protected $attributesType = MerchantReviewAttributes::class;
  protected $attributesDataType = '';
  protected $customAttributesType = CustomAttribute::class;
  protected $customAttributesDataType = 'array';
  /**
   * @var string
   */
  public $dataSource;
  /**
   * @var string
   */
  public $merchantReviewId;
  protected $merchantReviewStatusType = MerchantReviewStatus::class;
  protected $merchantReviewStatusDataType = '';
  /**
   * @var string
   */
  public $name;

  /**
   * @param MerchantReviewAttributes
   */
  public function setAttributes(MerchantReviewAttributes $attributes)
  {
    $this->attributes = $attributes;
  }
  /**
   * @return MerchantReviewAttributes
   */
  public function getAttributes()
  {
    return $this->attributes;
  }
  /**
   * @param CustomAttribute[]
   */
  public function setCustomAttributes($customAttributes)
  {
    $this->customAttributes = $customAttributes;
  }
  /**
   * @return CustomAttribute[]
   */
  public function getCustomAttributes()
  {
    return $this->customAttributes;
  }
  /**
   * @param string
   */
  public function setDataSource($dataSource)
  {
    $this->dataSource = $dataSource;
  }
  /**
   * @return string
   */
  public function getDataSource()
  {
    return $this->dataSource;
  }
  /**
   * @param string
   */
  public function setMerchantReviewId($merchantReviewId)
  {
    $this->merchantReviewId = $merchantReviewId;
  }
  /**
   * @return string
   */
  public function getMerchantReviewId()
  {
    return $this->merchantReviewId;
  }
  /**
   * @param MerchantReviewStatus
   */
  public function setMerchantReviewStatus(MerchantReviewStatus $merchantReviewStatus)
  {
    $this->merchantReviewStatus = $merchantReviewStatus;
  }
  /**
   * @return MerchantReviewStatus
   */
  public function getMerchantReviewStatus()
  {
    return $this->merchantReviewStatus;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(MerchantReview::class, 'Google_Service_Merchant_MerchantReview');
