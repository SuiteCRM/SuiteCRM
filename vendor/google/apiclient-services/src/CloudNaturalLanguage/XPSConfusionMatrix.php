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

namespace Google\Service\CloudNaturalLanguage;

class XPSConfusionMatrix extends \Google\Collection
{
  protected $collection_key = 'sentimentLabel';
  /**
   * @var string[]
   */
  public $annotationSpecIdToken;
  /**
   * @var int[]
   */
  public $category;
  protected $rowType = XPSConfusionMatrixRow::class;
  protected $rowDataType = 'array';
  /**
   * @var int[]
   */
  public $sentimentLabel;

  /**
   * @param string[]
   */
  public function setAnnotationSpecIdToken($annotationSpecIdToken)
  {
    $this->annotationSpecIdToken = $annotationSpecIdToken;
  }
  /**
   * @return string[]
   */
  public function getAnnotationSpecIdToken()
  {
    return $this->annotationSpecIdToken;
  }
  /**
   * @param int[]
   */
  public function setCategory($category)
  {
    $this->category = $category;
  }
  /**
   * @return int[]
   */
  public function getCategory()
  {
    return $this->category;
  }
  /**
   * @param XPSConfusionMatrixRow[]
   */
  public function setRow($row)
  {
    $this->row = $row;
  }
  /**
   * @return XPSConfusionMatrixRow[]
   */
  public function getRow()
  {
    return $this->row;
  }
  /**
   * @param int[]
   */
  public function setSentimentLabel($sentimentLabel)
  {
    $this->sentimentLabel = $sentimentLabel;
  }
  /**
   * @return int[]
   */
  public function getSentimentLabel()
  {
    return $this->sentimentLabel;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(XPSConfusionMatrix::class, 'Google_Service_CloudNaturalLanguage_XPSConfusionMatrix');
