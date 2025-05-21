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

namespace Google\Service\CloudKMS;

class ChecksummedData extends \Google\Model
{
  /**
   * @var string
   */
  public $crc32cChecksum;
  /**
   * @var string
   */
  public $data;

  /**
   * @param string
   */
  public function setCrc32cChecksum($crc32cChecksum)
  {
    $this->crc32cChecksum = $crc32cChecksum;
  }
  /**
   * @return string
   */
  public function getCrc32cChecksum()
  {
    return $this->crc32cChecksum;
  }
  /**
   * @param string
   */
  public function setData($data)
  {
    $this->data = $data;
  }
  /**
   * @return string
   */
  public function getData()
  {
    return $this->data;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ChecksummedData::class, 'Google_Service_CloudKMS_ChecksummedData');
