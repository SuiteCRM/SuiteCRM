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

namespace Google\Service\Appengine;

class Reasons extends \Google\Model
{
  /**
   * @var string
   */
  public $abuse;
  /**
   * @var string
   */
  public $billing;
  /**
   * @var string
   */
  public $dataGovernance;
  /**
   * @var string
   */
  public $serviceActivation;
  /**
   * @var string
   */
  public $serviceManagement;

  /**
   * @param string
   */
  public function setAbuse($abuse)
  {
    $this->abuse = $abuse;
  }
  /**
   * @return string
   */
  public function getAbuse()
  {
    return $this->abuse;
  }
  /**
   * @param string
   */
  public function setBilling($billing)
  {
    $this->billing = $billing;
  }
  /**
   * @return string
   */
  public function getBilling()
  {
    return $this->billing;
  }
  /**
   * @param string
   */
  public function setDataGovernance($dataGovernance)
  {
    $this->dataGovernance = $dataGovernance;
  }
  /**
   * @return string
   */
  public function getDataGovernance()
  {
    return $this->dataGovernance;
  }
  /**
   * @param string
   */
  public function setServiceActivation($serviceActivation)
  {
    $this->serviceActivation = $serviceActivation;
  }
  /**
   * @return string
   */
  public function getServiceActivation()
  {
    return $this->serviceActivation;
  }
  /**
   * @param string
   */
  public function setServiceManagement($serviceManagement)
  {
    $this->serviceManagement = $serviceManagement;
  }
  /**
   * @return string
   */
  public function getServiceManagement()
  {
    return $this->serviceManagement;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Reasons::class, 'Google_Service_Appengine_Reasons');
