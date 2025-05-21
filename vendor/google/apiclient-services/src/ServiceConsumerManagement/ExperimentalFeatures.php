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

namespace Google\Service\ServiceConsumerManagement;

class ExperimentalFeatures extends \Google\Model
{
  /**
   * @var bool
   */
  public $protobufPythonicTypesEnabled;
  /**
   * @var bool
   */
  public $restAsyncIoEnabled;
  /**
   * @var bool
   */
  public $unversionedPackageDisabled;

  /**
   * @param bool
   */
  public function setProtobufPythonicTypesEnabled($protobufPythonicTypesEnabled)
  {
    $this->protobufPythonicTypesEnabled = $protobufPythonicTypesEnabled;
  }
  /**
   * @return bool
   */
  public function getProtobufPythonicTypesEnabled()
  {
    return $this->protobufPythonicTypesEnabled;
  }
  /**
   * @param bool
   */
  public function setRestAsyncIoEnabled($restAsyncIoEnabled)
  {
    $this->restAsyncIoEnabled = $restAsyncIoEnabled;
  }
  /**
   * @return bool
   */
  public function getRestAsyncIoEnabled()
  {
    return $this->restAsyncIoEnabled;
  }
  /**
   * @param bool
   */
  public function setUnversionedPackageDisabled($unversionedPackageDisabled)
  {
    $this->unversionedPackageDisabled = $unversionedPackageDisabled;
  }
  /**
   * @return bool
   */
  public function getUnversionedPackageDisabled()
  {
    return $this->unversionedPackageDisabled;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ExperimentalFeatures::class, 'Google_Service_ServiceConsumerManagement_ExperimentalFeatures');
