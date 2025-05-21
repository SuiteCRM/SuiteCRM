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

namespace Google\Service\OnDemandScanning;

class LayerDetails extends \Google\Collection
{
  protected $collection_key = 'baseImages';
  protected $baseImagesType = BaseImage::class;
  protected $baseImagesDataType = 'array';
  /**
   * @var string
   */
  public $command;
  /**
   * @var string
   */
  public $diffId;
  /**
   * @var int
   */
  public $index;

  /**
   * @param BaseImage[]
   */
  public function setBaseImages($baseImages)
  {
    $this->baseImages = $baseImages;
  }
  /**
   * @return BaseImage[]
   */
  public function getBaseImages()
  {
    return $this->baseImages;
  }
  /**
   * @param string
   */
  public function setCommand($command)
  {
    $this->command = $command;
  }
  /**
   * @return string
   */
  public function getCommand()
  {
    return $this->command;
  }
  /**
   * @param string
   */
  public function setDiffId($diffId)
  {
    $this->diffId = $diffId;
  }
  /**
   * @return string
   */
  public function getDiffId()
  {
    return $this->diffId;
  }
  /**
   * @param int
   */
  public function setIndex($index)
  {
    $this->index = $index;
  }
  /**
   * @return int
   */
  public function getIndex()
  {
    return $this->index;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(LayerDetails::class, 'Google_Service_OnDemandScanning_LayerDetails');
