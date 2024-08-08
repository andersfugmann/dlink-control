#!/bin/sh -e
DEV=/sys/class/gpio/gpio:ac_output_enable/value

ID=$(uci get system.@system[0].hostname | cut -d'.' -f1)
DOMAIN=$(uci get system.@system[0].hostname | cut -d'.' -f2-)
URL=https://dlink.${DOMAIN}

function power_off () {
    echo Error encounterd. Power off
    echo 0 > ${DEV}
}

trap power_off ERR

function update_script () {
    if [ "${NO_UPDATE}" != "" ]; then
        return
    fi
    md5=$(cat $0 | md5sum)
    new_md5=$(wget -q -O- ${URL}/scripts/update.sh | md5sum)
    if [ "${md5}" != "${new_md5}" ]; then
        echo New script available
        wget -q -O/tmp/update.sh ${URL}/scripts/update.sh
        chmod +x /tmp/update.sh
        if NO_UPDATE=1 /tmp/update.sh; then
            echo Installing new script
            mv /tmp/update.sh $0
            exit 0
        else
            echo Skipping update due to errors
        fi
    fi
}

function update () {
    STATE=$(cat $DEV)
    NEW_STATE=$(wget -q -O- --timeout=10 "${URL}/get?id=${ID}&state=${STATE}")
    case ${NEW_STATE} in
        "0"|"1")
            echo ${NEW_STATE} > ${DEV}
            if [ "${NEW_STATE}" != "${STATE}" ]; then
                echo "State change: ${STATE} -> ${NEW_STATE}"
                update
            fi
            ;;
        *)
            echo Illegal return value.
            return 1
            ;;
    esac
}



update_script

for f in $(seq 4); do
    update && exit 0
    echo retry
    sleep 2s
done

power_off
exit 1
