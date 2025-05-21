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

namespace Google\Service\CloudHealthcare;

class BlobStorageInfo extends \Google\Model
{
  /**
   * @var string
   */
  public $sizeBytes;
  /**
   * @var string
   */
  public $storageClass;
  /**
   * @var string
   */
  public $storageClassUpdateTime;

  /**
   * @param string
   */
  public function setSizeBytes($sizeBytes)
  {
    $this->sizeBytes = $sizeBytes;
  }
  /**
   * @return string
   */
  public function getSizeBytes()
  {
    return $this->sizeBytes;
  }
  /**
   * @param string
   */
  public function setStorageClass($storageClass)
  {
    $this->storageClass = $storageClass;
  }
  /**
   * @return string
   */
  public function getStorageClass()
  {
    return $this->storageClass;
  }
  /**
   * @param string
   */
  public function setStorageClassUpdateTime($storageClassUpdateTime)
  {
    $this->storageClassUpdateTime = $storageClassUpdateTime;
  }
  /**
   * @return string
   */
  public function getStorageClassUpdateTime()
  {
    return $this->storageClassUpdateTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BlobStorageInfo::class, 'Google_Service_CloudHealthcare_BlobStorageInfo');
