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

namespace Google\Service\Dfareporting;

class CartData extends \Google\Collection
{
  protected $collection_key = 'items';
  protected $itemsType = CartDataItem::class;
  protected $itemsDataType = 'array';
  /**
   * @var string
   */
  public $merchantFeedLabel;
  /**
   * @var string
   */
  public $merchantFeedLanguage;
  /**
   * @var string
   */
  public $merchantId;

  /**
   * @param CartDataItem[]
   */
  public function setItems($items)
  {
    $this->items = $items;
  }
  /**
   * @return CartDataItem[]
   */
  public function getItems()
  {
    return $this->items;
  }
  /**
   * @param string
   */
  public function setMerchantFeedLabel($merchantFeedLabel)
  {
    $this->merchantFeedLabel = $merchantFeedLabel;
  }
  /**
   * @return string
   */
  public function getMerchantFeedLabel()
  {
    return $this->merchantFeedLabel;
  }
  /**
   * @param string
   */
  public function setMerchantFeedLanguage($merchantFeedLanguage)
  {
    $this->merchantFeedLanguage = $merchantFeedLanguage;
  }
  /**
   * @return string
   */
  public function getMerchantFeedLanguage()
  {
    return $this->merchantFeedLanguage;
  }
  /**
   * @param string
   */
  public function setMerchantId($merchantId)
  {
    $this->merchantId = $merchantId;
  }
  /**
   * @return string
   */
  public function getMerchantId()
  {
    return $this->merchantId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CartData::class, 'Google_Service_Dfareporting_CartData');
