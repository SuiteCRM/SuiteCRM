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

namespace Google\Service\Cloudchannel;

class GoogleCloudChannelV1alpha1DateRange extends \Google\Model
{
  protected $invoiceEndDateType = GoogleTypeDate::class;
  protected $invoiceEndDateDataType = '';
  protected $invoiceStartDateType = GoogleTypeDate::class;
  protected $invoiceStartDateDataType = '';
  protected $usageEndDateTimeType = GoogleTypeDateTime::class;
  protected $usageEndDateTimeDataType = '';
  protected $usageStartDateTimeType = GoogleTypeDateTime::class;
  protected $usageStartDateTimeDataType = '';

  /**
   * @param GoogleTypeDate
   */
  public function setInvoiceEndDate(GoogleTypeDate $invoiceEndDate)
  {
    $this->invoiceEndDate = $invoiceEndDate;
  }
  /**
   * @return GoogleTypeDate
   */
  public function getInvoiceEndDate()
  {
    return $this->invoiceEndDate;
  }
  /**
   * @param GoogleTypeDate
   */
  public function setInvoiceStartDate(GoogleTypeDate $invoiceStartDate)
  {
    $this->invoiceStartDate = $invoiceStartDate;
  }
  /**
   * @return GoogleTypeDate
   */
  public function getInvoiceStartDate()
  {
    return $this->invoiceStartDate;
  }
  /**
   * @param GoogleTypeDateTime
   */
  public function setUsageEndDateTime(GoogleTypeDateTime $usageEndDateTime)
  {
    $this->usageEndDateTime = $usageEndDateTime;
  }
  /**
   * @return GoogleTypeDateTime
   */
  public function getUsageEndDateTime()
  {
    return $this->usageEndDateTime;
  }
  /**
   * @param GoogleTypeDateTime
   */
  public function setUsageStartDateTime(GoogleTypeDateTime $usageStartDateTime)
  {
    $this->usageStartDateTime = $usageStartDateTime;
  }
  /**
   * @return GoogleTypeDateTime
   */
  public function getUsageStartDateTime()
  {
    return $this->usageStartDateTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudChannelV1alpha1DateRange::class, 'Google_Service_Cloudchannel_GoogleCloudChannelV1alpha1DateRange');
