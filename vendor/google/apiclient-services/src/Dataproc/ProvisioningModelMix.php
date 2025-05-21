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

namespace Google\Service\Dataproc;

class ProvisioningModelMix extends \Google\Model
{
  /**
   * @var int
   */
  public $standardCapacityBase;
  /**
   * @var int
   */
  public $standardCapacityPercentAboveBase;

  /**
   * @param int
   */
  public function setStandardCapacityBase($standardCapacityBase)
  {
    $this->standardCapacityBase = $standardCapacityBase;
  }
  /**
   * @return int
   */
  public function getStandardCapacityBase()
  {
    return $this->standardCapacityBase;
  }
  /**
   * @param int
   */
  public function setStandardCapacityPercentAboveBase($standardCapacityPercentAboveBase)
  {
    $this->standardCapacityPercentAboveBase = $standardCapacityPercentAboveBase;
  }
  /**
   * @return int
   */
  public function getStandardCapacityPercentAboveBase()
  {
    return $this->standardCapacityPercentAboveBase;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ProvisioningModelMix::class, 'Google_Service_Dataproc_ProvisioningModelMix');
