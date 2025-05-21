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

namespace Google\Service\Networkconnectivity;

class Filter extends \Google\Model
{
  /**
   * @var string
   */
  public $destRange;
  /**
   * @var string
   */
  public $ipProtocol;
  /**
   * @var string
   */
  public $protocolVersion;
  /**
   * @var string
   */
  public $srcRange;

  /**
   * @param string
   */
  public function setDestRange($destRange)
  {
    $this->destRange = $destRange;
  }
  /**
   * @return string
   */
  public function getDestRange()
  {
    return $this->destRange;
  }
  /**
   * @param string
   */
  public function setIpProtocol($ipProtocol)
  {
    $this->ipProtocol = $ipProtocol;
  }
  /**
   * @return string
   */
  public function getIpProtocol()
  {
    return $this->ipProtocol;
  }
  /**
   * @param string
   */
  public function setProtocolVersion($protocolVersion)
  {
    $this->protocolVersion = $protocolVersion;
  }
  /**
   * @return string
   */
  public function getProtocolVersion()
  {
    return $this->protocolVersion;
  }
  /**
   * @param string
   */
  public function setSrcRange($srcRange)
  {
    $this->srcRange = $srcRange;
  }
  /**
   * @return string
   */
  public function getSrcRange()
  {
    return $this->srcRange;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Filter::class, 'Google_Service_Networkconnectivity_Filter');
