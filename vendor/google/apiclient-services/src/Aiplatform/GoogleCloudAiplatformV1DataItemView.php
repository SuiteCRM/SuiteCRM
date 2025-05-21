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

namespace Google\Service\Aiplatform;

class GoogleCloudAiplatformV1DataItemView extends \Google\Collection
{
  protected $collection_key = 'annotations';
  protected $annotationsType = GoogleCloudAiplatformV1Annotation::class;
  protected $annotationsDataType = 'array';
  protected $dataItemType = GoogleCloudAiplatformV1DataItem::class;
  protected $dataItemDataType = '';
  /**
   * @var bool
   */
  public $hasTruncatedAnnotations;

  /**
   * @param GoogleCloudAiplatformV1Annotation[]
   */
  public function setAnnotations($annotations)
  {
    $this->annotations = $annotations;
  }
  /**
   * @return GoogleCloudAiplatformV1Annotation[]
   */
  public function getAnnotations()
  {
    return $this->annotations;
  }
  /**
   * @param GoogleCloudAiplatformV1DataItem
   */
  public function setDataItem(GoogleCloudAiplatformV1DataItem $dataItem)
  {
    $this->dataItem = $dataItem;
  }
  /**
   * @return GoogleCloudAiplatformV1DataItem
   */
  public function getDataItem()
  {
    return $this->dataItem;
  }
  /**
   * @param bool
   */
  public function setHasTruncatedAnnotations($hasTruncatedAnnotations)
  {
    $this->hasTruncatedAnnotations = $hasTruncatedAnnotations;
  }
  /**
   * @return bool
   */
  public function getHasTruncatedAnnotations()
  {
    return $this->hasTruncatedAnnotations;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1DataItemView::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1DataItemView');
