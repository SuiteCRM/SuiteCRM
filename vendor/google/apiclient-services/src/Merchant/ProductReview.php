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

class ProductReview extends \Google\Collection
{
  protected $collection_key = 'customAttributes';
  protected $attributesType = ProductReviewAttributes::class;
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
  public $name;
  /**
   * @var string
   */
  public $productReviewId;
  protected $productReviewStatusType = ProductReviewStatus::class;
  protected $productReviewStatusDataType = '';

  /**
   * @param ProductReviewAttributes
   */
  public function setAttributes(ProductReviewAttributes $attributes)
  {
    $this->attributes = $attributes;
  }
  /**
   * @return ProductReviewAttributes
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
   * @param string
   */
  public function setProductReviewId($productReviewId)
  {
    $this->productReviewId = $productReviewId;
  }
  /**
   * @return string
   */
  public function getProductReviewId()
  {
    return $this->productReviewId;
  }
  /**
   * @param ProductReviewStatus
   */
  public function setProductReviewStatus(ProductReviewStatus $productReviewStatus)
  {
    $this->productReviewStatus = $productReviewStatus;
  }
  /**
   * @return ProductReviewStatus
   */
  public function getProductReviewStatus()
  {
    return $this->productReviewStatus;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ProductReview::class, 'Google_Service_Merchant_ProductReview');
