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

class GoogleCloudRetailV2SearchRequestConversationalSearchSpecUserAnswerSelectedAnswer extends \Google\Collection
{
  protected $collection_key = 'productAttributeValues';
  protected $productAttributeValueType = GoogleCloudRetailV2ProductAttributeValue::class;
  protected $productAttributeValueDataType = '';
  protected $productAttributeValuesType = GoogleCloudRetailV2ProductAttributeValue::class;
  protected $productAttributeValuesDataType = 'array';

  /**
   * @param GoogleCloudRetailV2ProductAttributeValue
   */
  public function setProductAttributeValue(GoogleCloudRetailV2ProductAttributeValue $productAttributeValue)
  {
    $this->productAttributeValue = $productAttributeValue;
  }
  /**
   * @return GoogleCloudRetailV2ProductAttributeValue
   */
  public function getProductAttributeValue()
  {
    return $this->productAttributeValue;
  }
  /**
   * @param GoogleCloudRetailV2ProductAttributeValue[]
   */
  public function setProductAttributeValues($productAttributeValues)
  {
    $this->productAttributeValues = $productAttributeValues;
  }
  /**
   * @return GoogleCloudRetailV2ProductAttributeValue[]
   */
  public function getProductAttributeValues()
  {
    return $this->productAttributeValues;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudRetailV2SearchRequestConversationalSearchSpecUserAnswerSelectedAnswer::class, 'Google_Service_CloudRetail_GoogleCloudRetailV2SearchRequestConversationalSearchSpecUserAnswerSelectedAnswer');
