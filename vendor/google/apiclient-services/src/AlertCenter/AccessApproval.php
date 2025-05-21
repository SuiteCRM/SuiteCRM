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

namespace Google\Service\AlertCenter;

class AccessApproval extends \Google\Collection
{
  protected $collection_key = 'tickets';
  /**
   * @var string[]
   */
  public $justificationReason;
  /**
   * @var string
   */
  public $officeLocation;
  /**
   * @var string[]
   */
  public $products;
  /**
   * @var string
   */
  public $requestId;
  /**
   * @var string
   */
  public $scope;
  protected $ticketsType = SupportTicket::class;
  protected $ticketsDataType = 'array';

  /**
   * @param string[]
   */
  public function setJustificationReason($justificationReason)
  {
    $this->justificationReason = $justificationReason;
  }
  /**
   * @return string[]
   */
  public function getJustificationReason()
  {
    return $this->justificationReason;
  }
  /**
   * @param string
   */
  public function setOfficeLocation($officeLocation)
  {
    $this->officeLocation = $officeLocation;
  }
  /**
   * @return string
   */
  public function getOfficeLocation()
  {
    return $this->officeLocation;
  }
  /**
   * @param string[]
   */
  public function setProducts($products)
  {
    $this->products = $products;
  }
  /**
   * @return string[]
   */
  public function getProducts()
  {
    return $this->products;
  }
  /**
   * @param string
   */
  public function setRequestId($requestId)
  {
    $this->requestId = $requestId;
  }
  /**
   * @return string
   */
  public function getRequestId()
  {
    return $this->requestId;
  }
  /**
   * @param string
   */
  public function setScope($scope)
  {
    $this->scope = $scope;
  }
  /**
   * @return string
   */
  public function getScope()
  {
    return $this->scope;
  }
  /**
   * @param SupportTicket[]
   */
  public function setTickets($tickets)
  {
    $this->tickets = $tickets;
  }
  /**
   * @return SupportTicket[]
   */
  public function getTickets()
  {
    return $this->tickets;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AccessApproval::class, 'Google_Service_AlertCenter_AccessApproval');
