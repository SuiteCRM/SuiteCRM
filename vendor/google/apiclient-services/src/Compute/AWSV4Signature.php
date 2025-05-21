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

namespace Google\Service\Compute;

class AWSV4Signature extends \Google\Model
{
  /**
   * @var string
   */
  public $accessKey;
  /**
   * @var string
   */
  public $accessKeyId;
  /**
   * @var string
   */
  public $accessKeyVersion;
  /**
   * @var string
   */
  public $originRegion;

  /**
   * @param string
   */
  public function setAccessKey($accessKey)
  {
    $this->accessKey = $accessKey;
  }
  /**
   * @return string
   */
  public function getAccessKey()
  {
    return $this->accessKey;
  }
  /**
   * @param string
   */
  public function setAccessKeyId($accessKeyId)
  {
    $this->accessKeyId = $accessKeyId;
  }
  /**
   * @return string
   */
  public function getAccessKeyId()
  {
    return $this->accessKeyId;
  }
  /**
   * @param string
   */
  public function setAccessKeyVersion($accessKeyVersion)
  {
    $this->accessKeyVersion = $accessKeyVersion;
  }
  /**
   * @return string
   */
  public function getAccessKeyVersion()
  {
    return $this->accessKeyVersion;
  }
  /**
   * @param string
   */
  public function setOriginRegion($originRegion)
  {
    $this->originRegion = $originRegion;
  }
  /**
   * @return string
   */
  public function getOriginRegion()
  {
    return $this->originRegion;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AWSV4Signature::class, 'Google_Service_Compute_AWSV4Signature');
