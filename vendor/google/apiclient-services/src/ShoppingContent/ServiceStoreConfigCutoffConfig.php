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

namespace Google\Service\ShoppingContent;

class ServiceStoreConfigCutoffConfig extends \Google\Model
{
  protected $localCutoffTimeType = ServiceStoreConfigCutoffConfigLocalCutoffTime::class;
  protected $localCutoffTimeDataType = '';
  /**
   * @var bool
   */
  public $noDeliveryPostCutoff;
  /**
   * @var string
   */
  public $storeCloseOffsetHours;

  /**
   * @param ServiceStoreConfigCutoffConfigLocalCutoffTime
   */
  public function setLocalCutoffTime(ServiceStoreConfigCutoffConfigLocalCutoffTime $localCutoffTime)
  {
    $this->localCutoffTime = $localCutoffTime;
  }
  /**
   * @return ServiceStoreConfigCutoffConfigLocalCutoffTime
   */
  public function getLocalCutoffTime()
  {
    return $this->localCutoffTime;
  }
  /**
   * @param bool
   */
  public function setNoDeliveryPostCutoff($noDeliveryPostCutoff)
  {
    $this->noDeliveryPostCutoff = $noDeliveryPostCutoff;
  }
  /**
   * @return bool
   */
  public function getNoDeliveryPostCutoff()
  {
    return $this->noDeliveryPostCutoff;
  }
  /**
   * @param string
   */
  public function setStoreCloseOffsetHours($storeCloseOffsetHours)
  {
    $this->storeCloseOffsetHours = $storeCloseOffsetHours;
  }
  /**
   * @return string
   */
  public function getStoreCloseOffsetHours()
  {
    return $this->storeCloseOffsetHours;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ServiceStoreConfigCutoffConfig::class, 'Google_Service_ShoppingContent_ServiceStoreConfigCutoffConfig');
