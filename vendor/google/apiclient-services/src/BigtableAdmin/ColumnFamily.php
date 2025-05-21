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

namespace Google\Service\BigtableAdmin;

class ColumnFamily extends \Google\Model
{
  protected $gcRuleType = GcRule::class;
  protected $gcRuleDataType = '';
  protected $statsType = ColumnFamilyStats::class;
  protected $statsDataType = '';
  protected $valueTypeType = Type::class;
  protected $valueTypeDataType = '';

  /**
   * @param GcRule
   */
  public function setGcRule(GcRule $gcRule)
  {
    $this->gcRule = $gcRule;
  }
  /**
   * @return GcRule
   */
  public function getGcRule()
  {
    return $this->gcRule;
  }
  /**
   * @param ColumnFamilyStats
   */
  public function setStats(ColumnFamilyStats $stats)
  {
    $this->stats = $stats;
  }
  /**
   * @return ColumnFamilyStats
   */
  public function getStats()
  {
    return $this->stats;
  }
  /**
   * @param Type
   */
  public function setValueType(Type $valueType)
  {
    $this->valueType = $valueType;
  }
  /**
   * @return Type
   */
  public function getValueType()
  {
    return $this->valueType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ColumnFamily::class, 'Google_Service_BigtableAdmin_ColumnFamily');
