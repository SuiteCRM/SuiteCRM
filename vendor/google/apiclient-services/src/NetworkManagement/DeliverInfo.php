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

namespace Google\Service\NetworkManagement;

class DeliverInfo extends \Google\Model
{
  /**
   * @var string
   */
  public $ipAddress;
  /**
   * @var string
   */
  public $pscGoogleApiTarget;
  /**
   * @var string
   */
  public $resourceUri;
  /**
   * @var string
   */
  public $storageBucket;
  /**
   * @var string
   */
  public $target;

  /**
   * @param string
   */
  public function setIpAddress($ipAddress)
  {
    $this->ipAddress = $ipAddress;
  }
  /**
   * @return string
   */
  public function getIpAddress()
  {
    return $this->ipAddress;
  }
  /**
   * @param string
   */
  public function setPscGoogleApiTarget($pscGoogleApiTarget)
  {
    $this->pscGoogleApiTarget = $pscGoogleApiTarget;
  }
  /**
   * @return string
   */
  public function getPscGoogleApiTarget()
  {
    return $this->pscGoogleApiTarget;
  }
  /**
   * @param string
   */
  public function setResourceUri($resourceUri)
  {
    $this->resourceUri = $resourceUri;
  }
  /**
   * @return string
   */
  public function getResourceUri()
  {
    return $this->resourceUri;
  }
  /**
   * @param string
   */
  public function setStorageBucket($storageBucket)
  {
    $this->storageBucket = $storageBucket;
  }
  /**
   * @return string
   */
  public function getStorageBucket()
  {
    return $this->storageBucket;
  }
  /**
   * @param string
   */
  public function setTarget($target)
  {
    $this->target = $target;
  }
  /**
   * @return string
   */
  public function getTarget()
  {
    return $this->target;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DeliverInfo::class, 'Google_Service_NetworkManagement_DeliverInfo');
